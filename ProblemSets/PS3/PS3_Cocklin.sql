
CREATE TABLE FL_insurance_sample(
"policyID" INTEGER, 
"statecode" CHAR,
 "county" CHAR,
 "eq_site_limit" REAL,
 "hu_site_limit" REAL,
 "fl_site_limit" REAL,
 "fr_site_limit" REAL,
 "tiv_2011" REAL,
 "tiv_2012" REAL,
 "eq_site_deductible" INTEGER,
 "hu_site_deductible" REAL,
 "fl_site_deductible" REAL,
 "fr_site_deductible",
 "point_latitude" REAL,
 "point_longitude" REAL,
 "line" CHAR,
 "construction" CHAR,
 "point_granularity" REAL
 );
 .mode csv
.import FL_insurance_sample.csv FL_insurance_sample
Select county FROM FL_insurance_sample LIMIT 10;
 SELECT county, COUNT(*) FROM FL_insurance_sample GROUP BY county;
 SELECT AVG(tiv_2012-tiv_2011) FROM FL_insurance_sample;
 SELECT construction,COUNT(*) FROM FL_insurance_sample GROUP BY construction;
 


