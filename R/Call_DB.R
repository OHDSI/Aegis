Call.DB<-function(server, ip, usr, pw, schema){
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=server,
                                                               server=ip,
                                                               schema=schema,
                                                               user=usr,
                                                               password=pw)
connection <<- DatabaseConnector::connect(connectionDetails)
connectionDetails <<- connectionDetails
#return(connection)
}
