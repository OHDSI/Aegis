Call.DB<-function(server, ip, usr, pw, Resultschema){
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=server,
                                                               server=ip,
                                                               schema=Resultschema,
                                                               user=usr,
                                                               password=pw)
connection <<- DatabaseConnector::connect(connectionDetails)
connectionDetails <<- connectionDetails
#return(connection)
}
