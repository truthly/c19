<h1 id="top">🦠🐘<code>c19</code></h1>

This is an unofficial mirror of the Public Health Agency of Sweden's (Folkhälsomyndigheten) COVID-19 historical death data.

The data is updated automatically at 14:10 everyday by the `update.R` script,
which fetches data from the Excel file published at https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/bekraftade-fall-i-sverige/

Read-only access to the PostgreSQL database at `c19.truthly.com` is publicly available without any password.

PostgreSQL is completely free and can be downloaded from https://www.postgresql.org/download/

Once installed, you can connect to the database by opening up Terminal and executing:

```sh
psql -U c19 -h c19.truthly.com c19
```

Once logged in to the PostgreSQL database, you can execute SQL queries using the `deaths` table:

```sql
SELECT
    MAX(report_date) AS last_report_date,
    MAX(deaths) AS max_per_day,
    SUM(deaths),
    MAX(death_date)-MIN(death_date) AS death_days
FROM deaths
WHERE report_date = (SELECT MAX(report_date) FROM deaths);
```

```
 last_report_date | max_per_day | sum  | death_days
------------------+-------------+------+------------
 2020-04-23       |         111 | 2006 |         43
(1 row)
```

Below are three graphs generated by the `lageffect.R` script in this repo.
![GraphA](https://github.com/truthly/c19/blob/master/graphs/2021-07-13a.png?raw=true "GraphA")
![GraphB](https://github.com/truthly/c19/blob/master/graphs/2021-07-13b.png?raw=true "GraphB")
![GraphC](https://github.com/truthly/c19/blob/master/graphs/2021-07-13c.png?raw=true "GraphC")
