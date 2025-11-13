# These internal functions are borrowed from DatabaseConnector

getSqlDataTypes <- function(column) {
  if (is.integer(column)) {
    return("INTEGER")
  } else if (is(column, "POSIXct") | is(column, "POSIXt")) {
    return("DATETIME2")
  } else if (is(column, "Date")) {
    return("DATE")
  } else if (bit64::is.integer64(column)) {
    return("BIGINT")
  } else if (is.numeric(column)) {
    return("FLOAT")
  } else {
    if (is.factor(column)) {
      maxLength <-
        max(suppressWarnings(nchar(
          stringr::str_conv(string = as.character(column), encoding = "UTF-8")
        )), na.rm = TRUE)
    } else if (all(is.na(column))) {
      maxLength <- NA
    } else {
      maxLength <-
        max(suppressWarnings(nchar(
          stringr::str_conv(string = as.character(column), encoding = "UTF-8")
        )), na.rm = TRUE)
    }
    if (is.na(maxLength) || maxLength <= 255) {
      return("VARCHAR(255)")
    } else {
      return(sprintf("VARCHAR(%s)", maxLength))
    }
  }
}

.sql.qescape <- function(s, identifier = FALSE, quote = "\"") {
  s <- as.character(s)
  if (identifier) {
    validIdx <- grepl("^[A-Za-z]+([A-Za-z0-9_]*)$", s)
    if (any(!validIdx)) {
      if (is.na(quote)) {
        abort(paste0(
          "The JDBC connection doesn't support quoted identifiers, but table/column name contains characters that must be quoted (",
          paste(s[!validIdx], collapse = ","),
          ")"
        ))
      }
      s[!validIdx] <- .sql.qescape(s[!validIdx], FALSE, quote)
    }
    return(s)
  }
  if (is.na(quote)) {
    quote <- ""
  }
  s <- gsub("\\\\", "\\\\\\\\", s)
  if (nchar(quote)) {
    s <- gsub(paste("\\", quote, sep = ""), paste("\\\\\\", quote, sep = ""), s, perl = TRUE)
  }
  paste(quote, s, quote, sep = "")
}


toStrings <- function(data, sqlDataTypes) {
  intIdx <- (sqlDataTypes == "INT")
  if (nrow(data) == 1) {
    result <- sapply(data, as.character)
    if (any(intIdx)) {
      result[intIdx] <- sapply(data[, intIdx], format, scientific = FALSE)
    }
    result <- paste("'", gsub("'", "''", result), "'", sep = "")
    result[is.na(data)] <- "NULL"
    return(as.data.frame(t(result), stringsAsFactors = FALSE))
  } else {
    result <- sapply(data, as.character)
    if (any(intIdx)) {
      result[, intIdx] <- sapply(data[, intIdx], format, scientific = FALSE)
    }
    result <- apply(result, FUN = function(x) paste("'", gsub("'", "''", x), "'", sep = ""), MARGIN = 2)
    result[is.na(data)] <- "NULL"
    return(result)
  }
}


.insertTableSql <- function(executionSettings, tableName, data) {

  #prep data for insert
  data <- data |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    as.data.frame()

  sqlDataTypes <- sapply(data, getSqlDataTypes)
  sqlTableDefinition <- paste(.sql.qescape(names(data), TRUE), sqlDataTypes, collapse = ", ")
  sqlTableName <- .sql.qescape(tableName, TRUE, quote = "")
  sqlFieldNames <- paste(.sql.qescape(names(data), TRUE), collapse = ",")


  dataTxt <- toStrings(data, sqlDataTypes)
  valuesString <- paste("(", paste(apply(dataTxt, MARGIN = 1, FUN = paste, collapse = ","), collapse = "),\n("), ")")

  insertSql <- "DROP TABLE IF EXISTS @table;
  CREATE TABLE @table (@definition);
  INSERT INTO @table (@fields) \nVALUES @values;" |>
    SqlRender::render(table = sqlTableName,
                      definition = sqlTableDefinition,
                      fields = sqlFieldNames,
                      values = valuesString) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  return(insertSql)

}
