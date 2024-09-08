# fib-timetable

Download your [FIB](https://www.fib.upc.edu/en) lecture and exams schedule
in iCalendar (`.ics`) format, ready to import into your favorite calendar application.

## Usage

First, access https://fib-timetable.hgsg.me and authorize the application.
An iCalendar object file (`.ics`) should have been downloaded. To import it
into your application, create a new calendar and refer to one of the following
guides:

- [Google Calendar](https://support.google.com/calendar/answer/37118) (follow step 2),
- [Apple Calendar](https://support.apple.com/guide/calendar/import-or-export-calendars-icl1023/mac) (read the "Import
  events into a calendar" section).

To update the timetable, download the iCalendar file again, recreate the calendar,
and import the file.

## Setup

You will need the following dependencies to build `fib-timetable`:

- rustc >= 1.63
- node >= 18.7

```bash
npm install
```

## Build

To build and run a local server, execute `npm run dev`.

## License

[MIT](LICENSE) &copy; [Hugo Sanz González](https://hgsg.me).
