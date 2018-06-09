GIS.DirectAgeAdjustment <- function(CDMschema, year){

targettab <- "person"
cdmDatabaseSchema <- CDMschema
year <- lubridate::year(year)

sql <- "
select person_id, location_id, gender_concept_id,
            case
when @year-year_of_birth >= 90 then 10
when @year-year_of_birth > 89 then 9
when @year-year_of_birth > 79 then 8
when @year-year_of_birth > 69 then 7
when @year-year_of_birth > 59 then 6
when @year-year_of_birth > 49 then 5
when @year-year_of_birth > 39 then 4
when @year-year_of_birth > 29 then 3
when @year-year_of_birth > 19 then 2
when @year-year_of_birth >= 10 then 1
when @year-year_of_birth <= 9 then 0
end as age_cat
into #temp
from @cdmDatabaseSchema.@targettab
where year_of_birth <= @year
order by age_cat desc


select count(a.person_id) as count, b.fact_id_1 as gadm_id, a.age_cat
from #temp a left join nhis_nsc.dbo.fact_relationship b on a.location_id=b.fact_id_2
where b.domain_concept_id_1 = 4083586
group by b.fact_id_1, a.age_cat
order by b.fact_id_1, a.age_cat

drop table #temp
"
sql <- SqlRender::renderSql(sql,
                            targettab=targettab,
                            cdmDatabaseSchema=cdmDatabaseSchema,
                            year=year)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
temp <- DatabaseConnector::querySql(connection, sql)
return(temp)
}

