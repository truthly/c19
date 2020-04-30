CREATE TABLE deaths_per_region (
region text NOT NULL,
report_date date NOT NULL,
deaths integer NOT NULL,
PRIMARY KEY (region, report_date)
);
