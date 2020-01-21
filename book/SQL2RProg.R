library(RMySQL)
library(dbConnect)

con = dbConnect(MySQL(), user = 'sql9318960', password = 'C11PHWYGI7',
                dbname = 'sql9318960', host = 'sql9.freesqldatabase.com')

dbListTables(con)

myquery <- "SELECT * FROM EMPLOYEES";
df <- dbGetQuery(con, myquery);

str(df);

dbDisconnect(con);
