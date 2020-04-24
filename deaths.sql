CREATE TABLE deaths (
report_date date NOT NULL,
death_date date NOT NULL,
deaths integer NOT NULL,
PRIMARY KEY (report_date, death_date)
);
