use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::AddAssign;

use chrono::{Datelike, Duration, NaiveDate, NaiveDateTime, NaiveTime, Utc, Weekday};
use icalendar::{Calendar, Component, Event, EventLike};
use reqwest::Client;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use worker::*;

use crate::env_var_url;

/// The FIB API "Schedule" response contents.
#[derive(Debug, Deserialize)]
struct TimetableResponse {
    #[serde(rename = "results")]
    sessions: Vec<Session>,
}

/// Fetches a resource from the FIB API.
async fn get_resource<T>(client: &Client, env: &Env, path: &str) -> Result<T>
where
    T: DeserializeOwned,
{
    let api_base = env_var_url(env, "FIB_API_BASE")?;
    let resource_url = api_base.join(path)?;

    client
        .get(resource_url)
        .send()
        .await
        // todo: clean up error handling
        .map_err(|e| Error::RustError(e.to_string()))?
        .json::<T>()
        .await
        .map_err(|e| Error::RustError(e.to_string()))
}

/// Fetches the schedule of a student from the FIB API.
async fn get_schedule(client: &Client, env: &Env) -> Result<Vec<Session>> {
    get_resource::<TimetableResponse>(client, env, "jo/classes/")
        .await
        .map(|mut resp| {
            merge_consecutive_sessions(&mut resp.sessions);
            resp.sessions
        })
}

/// Merges any two consecutive sessions of the same subject
/// for the same group.
fn merge_consecutive_sessions(sessions: &mut Vec<Session>) {
    // Sort sessions by weekday and start time.
    sessions.sort_unstable_by(|a, b| a.weekday.cmp(&b.weekday).then(a.start.cmp(&b.start)));

    // Filter out consecutive events, starting from the end.
    for i in (1..sessions.len()).rev() {
        let prev = &sessions[i - 1];
        let sess = &sessions[i];
        let prev_end = prev.start + Duration::from(prev.duration);
        if prev.weekday == sess.weekday
            && prev.subject == sess.subject
            && prev.group == sess.group
            && prev_end == sess.start
        {
            let duration_delta = sess.duration;
            let prev = &mut sessions[i - 1];
            prev.duration += duration_delta;
            sessions.remove(i);
        }
    }
}

/// The FIB API "Subjects" response contents.
#[derive(Debug, Deserialize)]
struct SubjectsResponse {
    #[serde(rename = "results")]
    subjects: Vec<Subject>,
}

/// Fetches the subjects taken by a student from the FIB API.
async fn get_subjects(client: &Client, env: &Env) -> Result<Vec<Subject>> {
    get_resource::<SubjectsResponse>(client, env, "jo/assignatures/")
        .await
        .map(|resp| resp.subjects)
}

/// The FIB API "Calendar" response contents.
#[derive(Debug, Deserialize)]
struct FibEventsResponse {
    #[serde(rename = "results")]
    events: Vec<FibEvent>,
}

/// Fetches the calendar events from the FIB API.
async fn get_events(client: &Client, env: &Env) -> Result<Vec<FibEvent>> {
    get_resource::<FibEventsResponse>(client, env, "events/")
        .await
        .map(|resp| resp.events)
}

/// The FIB API "Exams" response contents.
#[derive(Debug, Deserialize)]
struct ExamsResponse {
    #[serde(rename = "results")]
    exams: Vec<Exam>,
}

/// Fetches all scheduled exams for the semester starting at
/// or during the given date from the FIB API, including
/// those the current user is not taking.
async fn get_exams(client: &Client, env: &Env, date: NaiveDate) -> Result<Vec<Exam>> {
    let semester_kind = SemesterKind::at(date);
    let course_year = match semester_kind {
        SemesterKind::Fall => date.year(),
        SemesterKind::Spring => date.year() - 1,
    };
    let exams_path = format!(
        "quadrimestres/{}Q{}/examens/",
        course_year, semester_kind as u8
    );
    get_resource::<ExamsResponse>(client, env, &exams_path)
        .await
        .map(|resp| resp.exams)
}

/// The days of the week when classes can be scheduled.
const WEEKDAYS: [Weekday; 5] = [
    Weekday::Mon,
    Weekday::Tue,
    Weekday::Wed,
    Weekday::Thu,
    Weekday::Fri,
];

/// Loads the schedule of the student associated with the given
/// authenticated client, converts it to the iCalendar format,
/// and outputs its contents as an HTTP response.
pub async fn export_calendar(client: &Client, env: &Env) -> Result<Response> {
    let sessions = get_schedule(client, env).await?;

    let subjects = get_subjects(client, env).await?;
    let subject_names: HashMap<String, String> = subjects
        .into_iter()
        .map(|subj| (subj.code, subj.name))
        .collect();

    // Add a calendar event for each scheduled exam the current user
    // is taking.
    let now = Utc::now().date_naive();
    let mut calendar = Calendar::new();
    let exams: Vec<Exam> = get_exams(client, env, now)
        .await?
        .into_iter()
        // Ignore all exams the user doesn't need to take.
        .filter(|e| subject_names.contains_key(&e.subject))
        .collect();
    for exam in &exams {
        let subject_name = &subject_names[&exam.subject];
        let summary = format!("Exam on {subject_name}");
        let location = if exam.rooms.is_empty() {
            "Unknown room".to_string()
        } else {
            format!("Room(s) {}", exam.rooms)
        };
        let event = Event::new()
            .summary(&summary)
            .starts(exam.start)
            .ends(exam.end)
            .location(&location)
            .done();
        calendar.push(event);
    }

    // Unfortunately the FIB API doesn't expose a week-by-week calendar,
    // so we must determine the first and last day of class, and skip
    // every holiday listed in the calendar within that range.
    let events = get_events(client, env).await?;
    let semester = Semester::at(now, &events, &exams);

    for weekday in WEEKDAYS {
        // Find all course sessions that regularly take place on `weekday`.
        let sessions: Vec<&Session> = sessions
            .iter()
            .filter(|&s| Weekday::from(s.weekday) == weekday)
            .collect();
        // Iterate over all workdays falling in `weekday` in the semester.
        let date_iter = ClassDateIterator::new(&semester, weekday, events.iter());
        for date in date_iter {
            for session in &sessions {
                // Append an event to the calendar corresponding to the session
                // on `date`.
                let subject = subject_names
                    .get(&session.subject)
                    .expect("schedule should only have sessions of taken courses");
                let start = date.and_time(session.start);
                let location = if session.room.is_empty() {
                    "Unknown room".to_string()
                } else {
                    format!("Room {}", session.room)
                };
                let event = Event::new()
                    .summary(&format!("{}, grp. {}", subject, session.group))
                    .starts(start)
                    .ends(start + Duration::from(session.duration))
                    .location(&location)
                    .done();
                calendar.push(event);
            }
        }
    }
    let mut headers = Headers::new();
    headers.set("Content-Type", "text/calendar")?;
    // todo: see if we can avoid the string allocation.
    let response = Response::ok(calendar.to_string())?;
    Ok(response.with_headers(headers))
}

/// An iterator over the days within a [`Semester`]
/// in which the FIB has lectures.
struct ClassDateIterator<'a, I>
where
    I: Iterator<Item = &'a FibEvent>,
{
    /// The last emitted day.
    cur: NaiveDate,
    /// The last day of classes.
    end: NaiveDate,
    /// The list of events, which may affect the list of workdays.
    events: Peekable<I>,
    /// The semester in which all emitted days fall.
    semester: &'a Semester,
}

impl<'a, I> ClassDateIterator<'a, I>
where
    I: Iterator<Item = &'a FibEvent>,
{
    /// Creates an iterator over all the days of class on `weekday`
    /// within the given semester. The list of events may contain
    /// holidays, which may affect the list of workdays.
    fn new(semester: &'a Semester, weekday: Weekday, events: I) -> Self {
        // Find the previous day that falls on `weekday` before
        // the first day of the semester.
        let start_weekday = semester.start.weekday();
        let mut delta = weekday as isize - start_weekday as isize;
        if delta >= 0 {
            delta -= 7; // `cur` falls in the previous week.
        }
        Self {
            cur: semester.start + Duration::days(delta as i64),
            end: semester.end,
            events: events.peekable(),
            semester,
        }
    }
}

impl<'a, I> Iterator for ClassDateIterator<'a, I>
where
    I: Iterator<Item = &'a FibEvent>,
{
    type Item = NaiveDate;

    fn next(&mut self) -> Option<Self::Item> {
        self.cur += Duration::days(7);
        if self.cur <= self.end {
            // Check `cur` does not fall in the period for midterm exams,
            // in which no classes are scheduled.
            let (mid_start, mid_end) = self.semester.midterms_period;
            if mid_start <= self.cur && self.cur <= mid_end {
                return self.next(); // see if this period is over by next week.
            }

            // Check whether `cur` is a holiday by advancing the event
            // iterator up to the current date.
            loop {
                match self.events.peek() {
                    // Skip all events prior to `self.cur`.
                    Some(&e) if e.end.date() < self.cur => {
                        self.events.next();
                        continue;
                    }
                    // There is no event at the current date if we have
                    // already skipped every event prior to `self.cur`
                    // and the following event starts after `self.cur`.
                    Some(&e) if self.cur < e.start.date() => break,
                    Some(&e) => {
                        // The event `e` occurs during `self.cur`; check
                        // if it indicates a holiday.
                        self.events.next();
                        if e.name == "FESTIU" || e.name == "NO LECTIU" {
                            // Skip this week, since `self.cur` is a holiday.
                            return self.next();
                        } else {
                            // We have advanced the iterator; keep checking for
                            // events in `self.cur`.
                        }
                    }
                    // There are no more unconsumed events after `self.cur`.
                    None => break,
                }
            }
            // The current date is not marked as a holiday in the calendar.
            Some(self.cur)
        } else {
            None
        }
    }
}

/// A course session taught in the FIB.
#[derive(Debug, Eq, PartialEq, Deserialize)]
struct Session {
    /// The subject code.
    #[serde(rename = "codi_assig")]
    subject: String,
    /// The student group code.
    #[serde(rename = "grup")]
    group: String,
    /// The day of the week when the session takes place.
    #[serde(rename = "dia_setmana")]
    weekday: RawWeekday,
    /// The starting time of the session.
    #[serde(rename = "inici")]
    start: NaiveTime,
    /// The session duration.
    #[serde(rename = "durada")]
    duration: Hours,
    /// The room where the session takes place.
    #[serde(rename = "aules")]
    room: String,
}

/// The number of hours covered by a [`Session`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Deserialize)]
#[serde(transparent)]
struct Hours(u32);

impl From<Hours> for Duration {
    fn from(count: Hours) -> Self {
        Duration::hours(count.0 as i64)
    }
}

impl AddAssign for Hours {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

/// A day of the week.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
#[serde(transparent)]
struct RawWeekday(u8);

impl From<RawWeekday> for Weekday {
    fn from(value: RawWeekday) -> Self {
        // The API uses one-indexed values for specifying weekdays
        // starting from Monday.
        Weekday::try_from(value.0 - 1).expect("weekday index should be <= 7")
    }
}

/// A subject taught in the FIB.
#[derive(Debug, Deserialize)]
struct Subject {
    /// The subject identifier.
    #[serde(rename = "id")]
    code: String,
    /// The student group code.
    #[serde(rename = "grup")]
    group: String,
    /// The subject full name.
    #[serde(rename = "nom")]
    name: String,
}

/// A calendar event at the FIB.
#[derive(Debug, Deserialize)]
struct FibEvent {
    /// The event name.
    #[serde(rename = "nom")]
    name: String,
    /// The starting time of the event.
    #[serde(rename = "inici")]
    start: NaiveDateTime,
    /// The end time of the event.
    #[serde(rename = "fi")]
    end: NaiveDateTime,
}

/// An exam event at the FIB.
#[derive(Debug, Deserialize)]
struct Exam {
    /// The subject code.
    #[serde(rename = "assig")]
    subject: String,
    /// The rooms where the exam takes place.
    #[serde(rename = "aules")]
    rooms: String,
    /// The starting time of the exam.
    #[serde(rename = "inici")]
    start: NaiveDateTime,
    /// The end time of the exam.
    #[serde(rename = "fi")]
    end: NaiveDateTime,
}

/// An academic term in the FIB.
#[derive(Debug)]
struct Semester {
    /// The first day of classes.
    start: NaiveDate,
    /// The last day of classes.
    end: NaiveDate,
    /// The type of the academic semester.
    kind: SemesterKind,
    /// The date period when midterm exams take place (and thus no classes
    /// are scheduled), both inclusive.
    midterms_period: (NaiveDate, NaiveDate),
}

/// The type of academic term.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum SemesterKind {
    /// The fall semester, which usually starts in September.
    Fall = 1,
    /// The spring semester, which usually starts in January.
    Spring,
}

impl SemesterKind {
    /// Returns the type of the semester beginning after or
    /// in progress during the given date.
    pub fn at(date: NaiveDate) -> Self {
        match date.month() {
            8..=12 => Self::Fall,
            1..=7 => Self::Spring,
            _ => panic!("current month should be <= 12"),
        }
    }
}

/// Returns the first calendar date not before `date` falling on a [Monday].
///
/// [Monday]: `Weekday::Mon`
fn first_monday_after(date: NaiveDate) -> NaiveDate {
    date.iter_days()
        .find(|d| d.weekday() == Weekday::Mon)
        .expect("next Monday is after the last representable date")
}

impl Semester {
    /// Returns the semester beginning after or in progress during the given date,
    /// taking into account the FIB calendar events and scheduled exams.
    pub fn at(date: NaiveDate, events: &[FibEvent], exams: &[Exam]) -> Self {
        let kind = SemesterKind::at(date);
        let midterms_period = exams
            .iter()
            .map(|e| (e.start.date(), e.end.date()))
            .filter(|(s, _)| s.month() != 1 && s.month() != 6) // ignore finals
            .filter(|(s, _)| s.year() == date.year()) // only midterms in semester
            .reduce(|acc, (s, e)| (acc.0.min(s), acc.1.max(e)))
            .expect("`exams` should contain at least one exam in the current period");

        // The "CURS" FIB event specifies the period of the semester.
        let (start, end) = if let Some(semester) = events
            .iter()
            .find(|&e| e.start.year() == date.year() && e.name == "CURS")
        {
            (semester.start.date(), semester.end.date())
        } else {
            // The calendar doesn't specify the class period yet; take some sane
            // default dates.
            match kind {
                SemesterKind::Fall => (
                    first_monday_after(NaiveDate::from_ymd_opt(date.year(), 9, 4).unwrap()),
                    NaiveDate::from_ymd_opt(date.year(), 12, 24).unwrap(),
                ),
                SemesterKind::Spring => {
                    // Assume the class period begins on the first Monday after
                    // the end of the spring semester enrollment window.
                    let enrollment_end = events
                        .iter()
                        .find(|&e| {
                            e.start.year() == date.year()
                                && e.name == "INSCRIPCIO-INSTANCIES-CANVI-MATRICULA"
                        })
                        .map(|e| e.start.date())
                        .unwrap_or_else(|| NaiveDate::from_ymd_opt(date.year(), 2, 11).unwrap());
                    // Take the end of the class period to be one week before
                    // the first final exam, if any.
                    let first_final = exams
                        .iter()
                        .map(|e| e.start.date())
                        .find(|d| d.month() == 6)
                        .unwrap_or_else(|| NaiveDate::from_ymd_opt(date.year(), 5, 31).unwrap());
                    (
                        first_monday_after(enrollment_end),
                        first_final - Duration::days(7),
                    )
                }
            }
        };
        Self {
            start,
            end,
            kind,
            midterms_period,
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::{Duration, NaiveDate, NaiveTime, Weekday};

    use crate::calendar::{
        ClassDateIterator, Exam, FibEvent, Hours, RawWeekday, Semester, SemesterKind, Session,
    };

    #[test]
    fn parse_session() -> serde_json::Result<()> {
        let raw = r#"{
            "codi_assig": "SMDE-MIRI",
            "grup": "10",
            "dia_setmana": 1,
            "inici": "10:00",
            "durada": 1,
            "tipus": "T",
            "aules": "A6203",
            "idioma": "English"
        }"#;
        let session: Session = serde_json::from_str(raw)?;

        assert_eq!(session.subject, "SMDE-MIRI");
        assert_eq!(session.group, "10");
        assert_eq!(Weekday::from(session.weekday), Weekday::Mon);
        assert_eq!(session.start, NaiveTime::from_hms_opt(10, 0, 0).unwrap());
        assert_eq!(Duration::from(session.duration), Duration::hours(1));
        assert_eq!(session.room, "A6203");

        Ok(())
    }

    #[test]
    fn semester_kind() {
        assert_eq!(
            SemesterKind::at(NaiveDate::from_ymd_opt(2023, 8, 5).unwrap()),
            SemesterKind::Fall
        );
        assert_eq!(
            SemesterKind::at(NaiveDate::from_ymd_opt(2024, 3, 7).unwrap()),
            SemesterKind::Spring
        );
    }

    #[test]
    fn semester() -> serde_json::Result<()> {
        let events = r#"[
            {
                "nom": "CURS",
                "inici": "2023-09-07T00:00:00",
                "fi": "2023-12-22T00:00:00",
                "categoria": "ALTRES"
            }
        ]"#;
        let events: Vec<FibEvent> = serde_json::from_str(events)?;
        let exams: [Exam; 1] = serde_json::from_str(
            r#"[
            {
                "id": 29722,
                "assig": "RA-MIRI",
                "codi_upc": "270612",
                "aules": "",
                "inici": "2023-10-27T13:00:00",
                "fi": "2023-10-27T15:00:00",
                "quatr": 1,
                "curs": 2023,
                "pla": "MIRI",
                "tipus": "P",
                "comentaris": "",
                "eslaboratori": ""
            }
        ]"#,
        )?;

        let date = NaiveDate::from_ymd_opt(2023, 10, 1).unwrap();
        let semester = Semester::at(date, &events, &exams);

        assert_eq!(semester.start, NaiveDate::from_ymd_opt(2023, 9, 7).unwrap());
        assert_eq!(semester.end, NaiveDate::from_ymd_opt(2023, 12, 22).unwrap());
        assert_eq!(semester.kind, SemesterKind::Fall);

        Ok(())
    }

    #[test]
    fn day_iterator() -> serde_json::Result<()> {
        let semester = Semester {
            start: NaiveDate::from_ymd_opt(2023, 9, 7).unwrap(),
            end: NaiveDate::from_ymd_opt(2023, 10, 20).unwrap(),
            kind: SemesterKind::Fall,
            midterms_period: (
                NaiveDate::from_ymd_opt(2023, 10, 11).unwrap(),
                NaiveDate::from_ymd_opt(2023, 10, 15).unwrap(),
            ),
        };
        let events: [FibEvent; 3] = serde_json::from_str(
            r#"[
            {
                "nom": "FESTIU",
                "inici": "2023-09-11T00:00:00",
                "fi": "2023-09-11T00:00:00",
                "categoria": "CALENDARI"
            },
            {
                "nom": "LIMIT-SEGUIMENT-TFG-OCTUBRE",
                "inici": "2023-09-15T00:00:00",
                "fi": "2023-09-15T00:00:00",
                "categoria": "TFE"
            },
            {
                "nom": "FESTIU",
                "inici": "2023-09-25T00:00:00",
                "fi": "2023-09-25T00:00:00",
                "categoria": "CALENDARI"
            }
        ]"#,
        )?;
        let mut iter = ClassDateIterator::new(&semester, Weekday::Mon, events.iter());
        assert_eq!(iter.cur, NaiveDate::from_ymd_opt(2023, 9, 4).unwrap());

        assert_eq!(
            iter.next(),
            Some(NaiveDate::from_ymd_opt(2023, 9, 18).unwrap())
        );
        assert_eq!(
            iter.next(),
            Some(NaiveDate::from_ymd_opt(2023, 10, 2).unwrap())
        );
        assert_eq!(
            iter.next(),
            Some(NaiveDate::from_ymd_opt(2023, 10, 9).unwrap())
        );

        Ok(())
    }

    #[test]
    fn merge_consecutive_sessions() -> serde_json::Result<()> {
        let mut sessions: Vec<Session> = serde_json::from_str(
            r#"[
            {
                "codi_assig": "SMDE-MIRI",
                "grup": "10",
                "dia_setmana": 1,
                "inici": "10:00",
                "durada": 1,
                "tipus": "T",
                "aules": "A6203",
                "idioma": "English"
            },
            {
                "codi_assig": "SMDE-MIRI",
                "grup": "10",
                "dia_setmana": 1,
                "inici": "11:00",
                "durada": 1,
                "tipus": "P",
                "aules": "A6203",
                "idioma": "English"
            }
        ]"#,
        )?;

        super::merge_consecutive_sessions(&mut sessions);
        assert_eq!(
            sessions,
            vec![Session {
                subject: "SMDE-MIRI".to_string(),
                group: "10".to_string(),
                weekday: RawWeekday(1),
                start: NaiveTime::from_hms_opt(10, 0, 0).unwrap(),
                duration: Hours(2),
                room: "A6203".to_string(),
            }]
        );
        Ok(())
    }
}
