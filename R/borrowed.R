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



.bindCodesetQueries <- function(conceptSet, codesetTable) {

  # get distinct concept sets from capr
  distinctConceptSetsMeta <- .caprToMetaTable(conceptSet) |>
    dplyr::distinct(id, csId, .keep_all = TRUE)

  # get ids to pluck
  idsToPluck <- distinctConceptSetsMeta |>
    dplyr::pull(rowId) |>
    as.integer()

  # csIds to Use
  csIdsToUse <- distinctConceptSetsMeta |>
    dplyr::pull(csId) |>
    as.integer()

  #pluch concept sets to the unique ones
  distinctConceptSetsToUse <- conceptSet[idsToPluck]

  # turn list of caprs to list of dfs with descendants
  csTb <- purrr::map(distinctConceptSetsToUse, ~exp_to_table(.x))

  # turn list of dfs into query
  ll <- purrr::map2(csTb, csIdsToUse, ~build_codeset_query(tb = .x, id = .y))

  # parameterize query
  set <- paste(ll, collapse = "\nUNION ALL\n")
  final_query <- glue::glue(
    "-- Create Codesets
    CREATE TABLE {codesetTable} (
        codeset_id int NOT NULL,
        concept_id bigint NOT NULL
    )
    ;

    INSERT INTO {codesetTable} (codeset_id, concept_id)
    {set}
    ;"
    )

  return(final_query)
}

# Capr helper functions to convert CS to query ----------------

.caprToMetaTable <- function(caprCs) {
  # make a table identifying the codeset id for the query, unique to each cs
  tb <- tibble::tibble(
    id = purrr::map_chr(caprCs, ~.x@id),
    name = purrr::map_chr(caprCs, ~.x@Name)
  ) |>
    dplyr::mutate(
      csId = dplyr::dense_rank(id)
    ) |>
    tibble::rownames_to_column(var = "rowId")
  return(tb)
}

build_codeset_query <- function(tb, id){

  # get concepts to exclude
  concepts_to_exclude <- tb |> dplyr::filter(is_excluded)
  # get concepts to include
  concepts_to_include <-  tb |> dplyr::filter(!is_excluded)

  concept_set_query <- include_concept_query(concepts_to_include)

  if (nrow(concepts_to_exclude) > 0) {
    exclude_query <- exclude_concept_query(concepts_to_exclude = concepts_to_exclude)
  } else(
    exclude_query = ""
  )

  codeset_query <- glue::glue("
  SELECT {id} as codeset_id, c.concept_id FROM
  (
  {concept_set_query}
  {exclude_query}
  ) C")
  return(codeset_query)
}

conceptSetQuerySql <- function(conceptIds) {
  conceptIds <- paste(conceptIds, collapse = ", ")
  sql <- glue::glue("select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in ({conceptIds})")
  return(sql)
}


conceptSetDescendantsSql <- function(conceptIds) {
  conceptIds <- paste(conceptIds, collapse = ", ")
  sql <- glue::glue(
    "select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in ({conceptIds})
  and c.invalid_reason is null"
  )
  return(sql)
}

conceptSetIncludeSql <- function(includeQuery) {
  sql <- glue::glue("select distinct I.concept_id
  FROM
  (
  {includeQuery}
  ) I")
  return(sql)
}


conceptSetExcludeSql <- function(excludeQuery) {
  sql <- glue::glue("LEFT JOIN
  (
  {excludeQuery}
  ) E ON I.concept_id = E.concept_id
  WHERE E.concept_id is null"
  )
  return(sql)
}

conceptSetMappedSql <- function(conceptSetQuery) {
  sql <- glue::glue("select distinct cr.concept_id_1 as concept_id
  FROM
  (
  {conceptSetQuery}
  ) C
  join @vocabulary_database_schema.concept_relationship cr
    on C.concept_id = cr.concept_id_2 and cr.relationship_id = 'Maps to' and cr.invalid_reason IS NULL")
  return(sql)
}

# convert concept set to table -----------------
exp_to_table <- function(cs) {
  tb <- tibble::tibble(
    concept_id = purrr::map_int(cs@Expression, ~.x@Concept@concept_id),
    is_excluded = purrr::map_lgl(cs@Expression, ~.x@isExcluded),
    include_descendants = purrr::map_lgl(cs@Expression, ~.x@includeDescendants),
    include_mapped = purrr::map_lgl(cs@Expression, ~.x@includeMapped)
  )
  return(tb)
}

# Build Queries ---------------------
include_concept_query <- function(concepts_to_include) {
  concept_set_query <- conceptSetQuerySql(conceptIds = concepts_to_include$concept_id)

  if (any(concepts_to_include$include_descendants)) {
    include_decendant_ids <- concepts_to_include |> dplyr::filter(include_descendants) |> dplyr::pull(concept_id)
    include_descendants_query <- conceptSetDescendantsSql(include_decendant_ids)
    concept_set_query <- glue::glue("{concept_set_query} \n UNION \n {include_descendants_query}")
  }

  final_query <- conceptSetIncludeSql(includeQuery = concept_set_query)

  return(final_query)
}

mapped_concept_query <- function(mapped_concepts) {

  concept_set_query <- conceptSetQuerySql(conceptIds = mapped_concepts$concept_id)

  if (any(mapped_concepts$include_descendants)) {
    include_decendant_ids <- mapped_concepts |> dplyr::filter(include_descendants) |> dplyr::pull(concept_id)
    include_descendants_query <- conceptSetDescendantsSql(include_decendant_ids)
    concept_set_query <- glue::glue("{concept_set_query} \n UNION \n {include_descendants_query}")
  }

  final_query <- conceptSetMappedSql(conceptSetQuery = concept_set_query)
  return(final_query)
}

exclude_concept_query <- function(concepts_to_exclude) {

  concept_set_query <- conceptSetQuerySql(conceptIds = concepts_to_exclude$concept_id)

  if (any(concepts_to_exclude$include_descendants)) {
    include_decendant_ids <- concepts_to_exclude |> dplyr::filter(include_descendants) |> dplyr::pull(concept_id)
    include_descendants_query <- conceptSetDescendantsSql(include_decendant_ids)
    concept_set_query <- glue::glue("{concept_set_query} \n UNION \n {include_descendants_query}")
  }

  final_query <- conceptSetExcludeSql(excludeQuery = concept_set_query)
  return(final_query)
}