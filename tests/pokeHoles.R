pokeHoles <- function(table, column, qty) {
    try(dbGetQuery(teaenv$con, "drop table precut"))
    dbGetQuery(teaenv$con, paste("create table precut as select * from ", table))
    table_len <-dbGetQuery(teaenv$con, paste("select count(*) from", table))[[1]]
    rowids <- floor(runif(table_len*qty)*table_len)

    lapply(rowids, function(row){
                  dbGetQuery(teaenv$con, paste("update ", table, "set", column, "=NULL where rowid=", row))
    })

    # A hack; automatic editing like this is introduced below.
    dbGetQuery(teaenv$con, paste("update",  table, "set", column, "=null where", column, "<0"))
}
