#-------------------------------------------------------------------------------
#
# Package templates
#
# Miscellaneous utility functions
#
# Hidden dependency: stringr in tmpl.split.csv()
#
# Sergei Izrailev, 2011-2012
#-------------------------------------------------------------------------------

#' Converts x to the given type. 
#' \itemize{
#' \item \code{binary} maps to integer
#' \item \code{logical} is false if the value of \code{x} is any of \code{NA}, 
#'                      zero (after conversion with \code{as.numeric}), 
#'                      \code{"FALSE", "NO", "F", "N"} (all case-insensitive).
#' \item \code{double}, integer, numeric map to values with \code{as.<type>()} calls.
#' \item \code{date} is converted with \code{as.Date()}
#' \item \code{int64} is cast to class \code{\link{int64}}
#' \item \code{literal} is treated as character and is assigned class \code{literal}. This is useful to 
#'       distinguish between strings that should and should not be enclosed in quotes when processed
#'       by other functions. The intent is that a \code{literal} will not be enclosed in quotes by \code{\link{tmpl.quote.sql}}. 
#' }
#' @name tmpl.cast
#' @aliases tmpl.cast
#' @title Type casts.
#' @rdname tmpl.cast
#' @examples
#' \dontrun{ 
#' tmpl.cast("12.3", "integer")
#' tmpl.cast("AMP_MASTER", "literal")
#' }
tmpl.cast <- function(x, type)
{
   if (is.na(match(str_trim(tolower(type)), c("integer", "character", "numeric", "double", "binary", "logical", "date", "int64", "literal"))) 
         || (type == "int64" && !exists("as.int64"))) stop(paste("tmpl.cast: unknown type", type))
   if (type == "integer")     return(as.integer(x))
   if (type == "character")   return(as.character(x))
   if (type == "numeric")     return(as.numeric(x))
   if (type == "double")      return(as.double(x))
   if (type == "binary")      return(as.integer(x))
   if (type == "logical")     return(!(is.na(x) | as.numeric(x) == 0 | toupper(x) == "FALSE" | toupper(x) == "F" | toupper(x) == "NO" | toupper(x) == "N"))
   if (type == "date")        return(tryCatch(as.Date(x), error = function(e) { stop(paste("tmpl.cast: invalid date '", x, "': ", e$message, sep="")) }))
   #if (type == "date")        return(as.Date(x))
   if (type == "int64")       return(as.int64(x))
   # literal is the same as character, but doesn't get enclosed in quotes downstream
   if (type == "literal")     { res <- as.character(x); class(res) <- "literal"; return(res) }    
}

#-------------------------------------------------------------------------------

#' \code{tmpl.literal} - Convert character elements to class \code{literal}. 
#' @param x A scalar, vector or list. For scalars and vectors, if \code{typeof(x)} returns \code{"character"},
#'          \code{x} is converted to class \code{character}. For lists, the same happens with each element 
#'          recursively using \code{lapply}.
#' @return \code{tmpl.literal} returns an object where character strings are cast to \code{literal}. 
#'         If a \code{data.frame} is supplied as an argument, returns a list, not a data frame. 
#' @usage tmpl.literal(mylist)
#' @rdname tmpl.cast
tmpl.literal <- function(x)
{
   if (is.list(x))
   {
      lapply(x, 
            function(c) 
            {
               if (typeof(c) == "character") return(tmpl.literal(c))
               else return(c)
            })
   }
   else
   {
      if (typeof(x) == "character") return(tmpl.cast(x, "literal"))
      else return(x)      
   }
}

#-------------------------------------------------------------------------------

#' \code{tmpl.quote} - Enclose a character string in quotes. 
#' @param s A character scalar or vector.
#' @param quote can be set to a single quote using "'". Default is double quotes.
#' @return \code{tmpl.quote} returns the string enclosed in quotes. If \code{s} is a vector, returns a vector of quoted strings.
#' @usage tmpl.quote(s, quote = '"')
#' @name tmpl.quote
#' @aliases tmpl.quote
#' @title Enclosing strings in quotes.
#' @rdname tmpl.quote
tmpl.quote <- function(s, quote = '"') 
{
   #paste('"', s, '"', sep="")
   paste(quote, s, quote, sep="")
}

#-------------------------------------------------------------------------------

#' \code{tmpl.quote.csv} - Converts the vector vec into a comma-separated list of elements enclosed in quotes.
#' @param vec A vector of character strings.
#' @examples 
#' \dontrun{
#' tmpl.quote.csv(c("aa", "bb", "cc"))
#' [1] "\"aa\", \"bb\", \"cc\""
#' }
#' @usage tmpl.quote.csv(vec, quote = '"')
#' @rdname tmpl.quote
tmpl.quote.csv <- function(vec, quote = '"')
{
   paste(unlist(lapply(vec, function(x) tmpl.quote(x, quote))), collapse=", ")
}

#-------------------------------------------------------------------------------

#' \code{tmpl.quote.sql} Quotes character strings in a way appropriate for direct inclusion in SQL
#' depending on their class. \code{character} class is enclosed in single quotes, \code{Date} 
#' class (see \code{as.Date} and \code{tmpl.cast}) has \code{::date} appended to the quoted string, while
#' class \code{literal} (see \code{tmpl.cast}) is returned unchanged. \code{NULL} and \code{NA} 
#' are converted to \code{"NULL"}. \code{logical} values are cast \code{as.integer}.
#' @examples
#' \dontrun{
#' 
#' # Example for tmpl.quote.sql() and tmpl.cast() 
#' dt <- tmpl.cast("2011-10-20", "date")
#' tab <- tmpl.cast("impression", "literal")
#' tagtype <- tmpl.cast("cmadj", "character")
#' sql <- paste("select count(*) from ", tmpl.quote.sql(tab), 
#'              " where file_date = ", tmpl.quote.sql(dt), 
#'              " and tag_type != ", tmpl.quote.sql(tagtype), sep="")
#' print(sql)
#' # [1] "select count(*) from impression where file_date = '2011-10-20'::date 
#' #      and tag_type != 'cmadj'"
#' }
#' @rdname tmpl.quote  
tmpl.quote.sql <- function(s) 
{ 
   if (length(s) > 1) ix.nulls <- is.na(s)   # can't have NULLs in a vector   
   else ix.nulls <- is.null(s) || is.na(s)   # must check for NULL first otherwise is.na() gives an error 
   if (class(s) == "literal") res <- as.character(s) 
   else if (class(s) == "Date") res <- paste("'", s, "'::date", sep="")
   else if (typeof(s) == "character") res <- paste("'", gsub("'", "''", s), "'", sep="") 
   else if (typeof(s) == "logical") res <- as.character(as.integer(s))
   else res <- as.character(s)
   if (any(ix.nulls)) res[ix.nulls] <- "NULL"
   return(res)
}

#-------------------------------------------------------------------------------

#' Substitutes parameters in \code{sql} with values defined in the \code{var.list}. 
#' Use \code{\link{tmpl.cast()}} to set proper data types. Note that substitution is recursive (left to right), 
#' so you can use vars inside other vars values, as long as the var order is correct. However, 
#' it is an error if a variable is a substring of another variable that follows in the substitution order
#' (see \code{\link{tmpl.varsub.conflicts}}).
#' @param var.list If not NULL, variables in \code{sql} are substituted with values defined in the \code{var.list}. 
#'        Use \code{tmpl.cast()} to set proper data types. Note that substitution is recursive (left to right), 
#'        so you can use vars inside other vars, as long as the var order is correct.
#' @param all.literal If \code{TRUE}, all strings in \code{var.list} are treated as literals, i.e., 
#'        they won't be enclosed in quotes. This is useful when there are no strings that need to be 
#'        quoted. 
#' @examples
#' \dontrun{
#' 
#' # Examples for tmpl.varsub.sql() 
#' 
#' # Mixed strings and literals.
#' query <- "select cookie_id, count(*) as cnt from MYTABLE 
#'           where date = THISDATE and run_id = THISRUNID and 
#'           type = THISSTRING group by cookie_id"
#' 
#' # THISSTRING is quoted, but MYTABLE is cast to literal.
#' tmpl.varsub.sql(query, var.list = list(THISDATE = as.Date('2012-04-01'), 
#'           THISSTRING = 'fnl', 
#'           MYTABLE = tmpl.cast('this_table_THISRUNID', 'literal'), 
#'           THISRUNID = 32))
#' [1] "select cookie_id, count(*) as cnt from this_table_32
#' where date = '2012-04-01'::date and run_id = 32 and
#' type = 'fnl' group by cookie_id"
#' 
#' # Same, but use the shorter tmpl.literal()
#' tmpl.varsub.sql(query, var.list = list(THISDATE = as.Date('2012-04-01'), 
#'           THISSTRING = 'fnl', 
#'           MYTABLE = tmpl.literal('this_table_THISRUNID'), 
#'           THISRUNID = 32))
#' 
#' # All character variables are literals.
#' query2 <- "select THISCOLNAME, count(*) as cnt from MYTABLE 
#'           where date = THISDATE and run_id = THISRUNID and 
#'           THISCOLNAME is not null group by THISCOLNAME"
#' 
#' # Cast the whole argument list as literal to convert all character variables at once.
#' tmpl.varsub.sql(query2, var.list = tmpl.literal(list(THISDATE = as.Date('2012-04-01'),
#'           THISCOLNAME = "cookie_id",
#'           MYTABLE = "this_table_THISRUNID",
#'           THISRUNID = 32)))
#' [1] "select cookie_id, count(*) as cnt from this_table_32          
#' where date = '2012-04-01'::date and run_id = 32 and 
#' cookie_id is not null group by cookie_id"
#' 
#' # Same result with passing a flag all.literal = TRUE.
#' tmpl.varsub.sql(query2, var.list = list(THISDATE = as.Date('2012-04-01'),
#'           THISCOLNAME = "cookie_id",
#'           MYTABLE = "this_table_THISRUNID",
#'           THISRUNID = 32), all.literal = TRUE)
#' }
#' @name tmpl.varsub.sql
#' @aliases tmpl.varsub.sql
#' @title Variable substitution in SQL queries.
#' @rdname tmpl.varsub.sql  
#' @seealso \code{\link{tmpl.nzquery}}, \code{\link{tmpl.cast}}, \code{\link{tmpl.quote.sql}}
tmpl.varsub.sql <- function(sql, var.list, all.literal = FALSE)
{
   if (is.null(var.list) || is.na(var.list) || length(var.list) == 0) return(sql)
   if (!is.list(var.list)) stop("tmpl.varsub.sql: expecting a list as the var.list parameter")
   if ((res = tmpl.varsub.conflicts(var.list)))
   {
      stop(paste("tmpl.varsub.sql: Variable ", tmpl.quote(attr(res, "var.name")), " conflicts with variables ", 
                  tmpl.quote.csv(attr(res, "var.conflict")), sep=""))   
   }
   if (all.literal) var.list <- tmpl.literal(var.list)
   for (j in 1:length(var.list))
   {
      sql <- gsub(names(var.list)[j], tmpl.quote.sql(var.list[[j]]), sql, fixed = TRUE)
   }
   return(sql)   
}

#-------------------------------------------------------------------------------

#' \code{tmpl.varsub.conflicts} Checks for any string substitutions in \code{var.list} that are 
#' substrings of other strings being substituted. 
#' @param var.list Substitution list. Only the names of the list items are used. The list is
#'        processed from beginning to the end, i.e., conflicts may only occur if the subsequent
#'        variable name is overwritten by substitution.
#' @param do.stop If \code{TRUE} stops with an error message. 
#' @return \code{tmpl.varsub.conflicts} returns \code{TRUE} if there are conflicts in substitution, 
#'         and \code{FALSE} otherwise. When \code{TRUE}, the return value's attributes 
#'         \code{var.name} and \code{var.conflict} contain the name of the conflicting variable
#'         and a vector of variable names with which there is a conflict.
#' @examples
#' \dontrun{
#' 
#' # Example for tmpl.varsub.conflicts() 
#' if ((res = tmpl.varsub.conflicts(list(AB=1, BAB="F", BBC="F", BBB=2, ABB=4))))
#' {
#'    stop(paste("Variable ", tmpl.quote(attr(res, "var.name")), " conflicts with variables ", 
#'                tmpl.quote.csv(attr(res, "var.conflict")), sep=""))   
#' } else
#' {
#'    "OK"
#' }
#' # Error: Variable "AB" conflicts with variables "BAB", "ABB"
#' }
#' @rdname tmpl.varsub.sql  
tmpl.varsub.conflicts <- function(var.list, do.stop = FALSE)
{
   if (!is.list(var.list)) stop("tmpl.varsub.conflicts: expecting a list as the var.list parameter")
   slist <- names(var.list)
   n <- length(slist)
   if (n < 2) return(FALSE) 
   for (j in 1 : (n - 1))
   {
      s <- slist[j]
      bad <- c()   # an array to accumulate conflicting variables
      for (i in (j + 1) : n)
      {
         snew <- gsub(s, "", slist[i])
         if (snew != slist[i]) 
         {
            if (do.stop) stop(paste("tmpl.varsub.conflicts: variable '", s, "' conflicts with '", slist[i], "'", sep=""))
            bad <- c(bad, slist[i])
         }
      }      
      if (length(bad) > 0)
      {
         res <- TRUE
         attr(res, "var.name") <- s
         attr(res, "var.conflict") <- bad
         return(res)            
      }
   }
   return(FALSE)
}

#-------------------------------------------------------------------------------

#' \code{tmpl.varsub.checknulls} Checks for any string substitutions of parameters
#' in \code{sql} with values in \code{var.list} that are \code{NULL} or \code{NA}.
#' This function provides the ability to perform extra error checking before running
#' \code{tmpl.varsub.sql} in cases when no NULLs are allowed in certain columns.
#' @param var.list Substitution list. The \code{NULL} or \code{NA} values of elements in 
#'        \code{var.list} that are not used in the string substitution are ignored.
#' @param do.stop If \code{TRUE} stops with an error message. 
#' @return \code{tmpl.varsub.checknulls} returns \code{TRUE} if there are nulls in substitution, 
#'         and \code{FALSE} otherwise. When \code{TRUE}, the return value's attribute
#'         \code{var.nulls} contains a vector of variable names that have \code{NULL} or 
#'         \code{NA} values and are used in substitution.
#' @examples
#' \dontrun{
#' 
#' # Example for tmpl.varsub.checknulls() 
#' if ((res = tmpl.varsub.checknulls("select * from TAB where val = VAL limit LIM", 
#'    list(AB=1, TAB=NA, BBC="F", BBB=NA, LIM=NULL, VAL = NA))))
#' {
#'    allowed.nulls <- c("VAL")
#'    var.nulls <- attr(res, "var.nulls")
#'    disallowed.nulls <- var.nulls[!( var.nulls %in% allowed.nulls )]
#'    if (length(disallowed.nulls) > 0)
#'       stop(paste("Substitution list contains variables that are NULL or NA: ", 
#'                   tmpl.quote.csv(disallowed.nulls), sep=""))   
#' } else
#' {
#'    "OK"
#' }
#' # Error: Substitution list contains variables that are NULL or NA: "TAB", "LIM"
#' }
#' @rdname tmpl.varsub.sql  
tmpl.varsub.checknulls <- function(sql, var.list, do.stop = FALSE)
{
   if (!is.list(var.list)) stop("tmpl.varsub.checknulls: expecting a list as the var.list parameter")
   slist <- names(var.list)
   n <- length(slist)
   ix.nulls <- unlist(lapply(var.list, function(x) as.logical(is.null(x) || is.na(x))))
   bad <- c()   # an array to accumulate null variables
   for (i in which(ix.nulls) )
   {
      # Check if there will be a substitution with NULL.
      sqlnew <- gsub(slist[i], "", sql)
      if (sqlnew != sql) 
      {
         if (do.stop) stop(paste("tmpl.varsub.checknulls: variable '", slist[i], "' is NULL or NA.", sep=""))
         bad <- c(bad, slist[i])
      }
   }
   if (length(bad) > 0)
   {
      res <- TRUE
      attr(res, "var.nulls") <- bad
      return(res)            
   }
   return(FALSE)
}

#-------------------------------------------------------------------------------

#' \code{tmpl.split.csv} - Takes a (comma) delimited list as a single string and returns a trimmed vector (was column.parse()).
#' @rdname tmpl.utils
tmpl.split.csv <- function(csv.line, delimiter = ',') 
{
   if (length(csv.line) > 1) stop("tmpl.split.csv: this function can only operate on scalars, use lapply() for vectors.")
   
   require(stringr)
   v <- vector(mode = "character") 
   if(!is.na(csv.line)) 
   {
      v <- unlist(str_split(csv.line, delimiter)) # split by delimiter
      v <- unlist(lapply(v, str_trim)) # trim
   }
   v
}

#-------------------------------------------------------------------------------

#' \code{tmpl.vmatch} - Returns the index of the first vector in lst that is equal to v or NA if 
#' a match was not found.
#' @rdname tmpl.utils
tmpl.vmatch <- function(v, lst)
{
   m <- which(unlist(lapply(lst, 
                     function(x) 
                     {
                        length(v) == length(x) && all(v == x)                  
                     })))
   if (length(m) == 0) return(NA)
   return(m[1])
}

#-------------------------------------------------------------------------------

#' \code{tmpl.file2char} - Reads in a text file, converts newlines to spaces and 
#' returns the resulting single character string.
#' @rdname tmpl.utils
tmpl.file2char <- function(file, collapse = " ")
{
   paste(scan(file, "character", sep = "\n"), collapse = collapse)   
}

#-------------------------------------------------------------------------------

#' Takes a list of columns of the same length (typically, a data frame) and 
#' the names of the columns, unique combinations of the values of which form groups. 
#' The groups are arbitrarily numbered from 1 to the number of groups. 
#' The function assigns each row to a group. Returns a list containing the group numbers
#' for the rows in lst, and unique combinations of the grouping columns' values
#' in the numbered order.
#'
#' @examples
#' \dontrun{
#'    lst = data.frame(a=c(1,1,2,3,2,1), b=c(1,1,1,1,1,1), c=c(0,1,0,1,0,1))
#'    grpcols = c("b", "c") 
#'    # There are 2 unique (b, c) combinations: (1, 0) and (1, 1). 
#'    tmpl.assign.groups(lst, grpcols)
#' $def
#'   b c
#' 1 1 0
#' 2 1 1
#'
#' $idx
#' [1] 1 2 1 2 1 2
#'}
#' @name tmpl.assign.groups
#' @aliases tmpl.assign.groups
#' @title Assignment of groups to unique combinations of values.
#' @rdname tmpl.assign.groups
tmpl.assign.groups <- function(lst, grpcols)
{
   if ((typeof(grpcols) != "character" && any(grpcols) > length(lst)) 
         || any(is.na(match(grpcols, names(lst)))))
   {
      stop("tmpl.assign.groups: grpcols don't match lst names or indices")    
   }
   na.marker <- "THIS IS REALLY NA"                                                 # can't handle NAs, must replace with placeholder      
   src.cols <- as.data.frame(lst[grpcols], stringsAsFactors=F)                      # just get the cols defining the group
   src.cols[is.na(src.cols)] <- na.marker                                           # replace NAs with strings
   group.def <- as.data.frame(t(unique(src.cols)), stringsAsFactors=F)              # definition of the group = unique combo of grpcols
   group.idx <- match(as.data.frame(t(src.cols), stringsAsFactors=F), group.def)    # group id for each row
   group.def[group.def == na.marker] <- NA                                          # restore NAs
   return(list(def=as.data.frame(t(group.def), stringsAsFactors=F),                 # transpose group.def back to column-oriented
               idx=group.idx))                                                      # and return
}

#-------------------------------------------------------------------------------

#' \code{tmpl.uid} - Generates a "unique" id, which is the number of days since the from.date times 1M.
#' The idea is to use a relatively short number to be used in file names and 
#' temporary tables. The time resolution is fairly high, but don't use this in an
#' inner loop of some calculation generating ids, rather, use the result of this function
#' as an offset. 
#'
#' The number of days is computed as a fraction. To make this random, use something like 
#'    tmpl.uid() + floor(runif(1) * 1000000) 
#' but then tmpl.uid.decode() won't apply.
#' 
#' To decrease chances of clobbering when executing in parallel, use the following before calling tmpl.uid():
#'    Sys.sleep(runif(1))  
#' However, the resolution of the time stamp should be sufficient as is. See for example
#'    for (x in 1:20) { print( as.character(tmpl.uid()) ) }
#' 
#' @param from.date - date in the YYYY-MM-DD format used as an offset to reduce the range of
#'             the resulting number. Something within a couple of years should be fine. Clearly,
#'             if the from.date changes, the result may not be unique. 
#' @name tmpl.uid
#' @aliases tmpl.uid
#' @title Unique identifilers. 
#' @rdname tmpl.uid
tmpl.uid <- function(from.date = "2012-04-01")
{
   as.int64(as.double(floor(1000000000*(Sys.time() - as.POSIXct(from.date)))))
}

#-------------------------------------------------------------------------------

#' \code{tmpl.uid.decode} - Converts the uid generated by tmpl.uid() into a timestamp in POSIXct format. 
#' @rdname tmpl.uid
tmpl.uid.decode <- function(uid, from.date = "2012-04-01")
{
   as.POSIXct(from.date) + uid / 100000 * 8.64  # 8.64 = 60 * 60 * 24 / 10000
}

#-------------------------------------------------------------------------------

#' \code{tmpl.get.option} - Checks if a given option exists using \code{getOption(opt)}. If so, returns
#' the option value, otherwise, returns the supplied \code{default}.
#' @rdname tmpl.utils
tmpl.get.option <- function(opt, default = NULL, caller = "")
{
   if (is.null(getOption(opt))) 
   {
      if (is.null(default)) stop(paste(caller, ": Couldn't find '", opt, "' in options, with no value provided as default.", sep="")) 
      return(default)
   }
   return(getOption(opt))
}

#-------------------------------------------------------------------------------

#' \code{tmpl.convert.cols} Converts columns in data frame \code{frm} to the types specified in the 
#' list \code{col.types} using \code{\link{tmpl.cast}}. The column names of the data frame are matched
#' with the names of the \code{col.types} list (case-insensitive) to determine the types.
#'
#' @param frm Data frame to be converted.
#' @param col.types A list specifying column names (as list item names)  and their types (as list items).
#'        Passing NULL will result in unchanged \code{frm}. The list can also be specified 
#'        in the \code{NZCOL.TYPES} option. Either an explicit argument or this option is required.
#' @examples 
#' \dontrun{
#' 
#' # Example for tmpl.convert.cols
#' frm <- tmpl.nzquery("select * from mm_instance", dsn = "NZMM")
#' class(frm$UID)
#' [1] "numeric"
#' 
#' ctypes <- list(uid = "int64", config_uid = "int64", tab_meta = "literal")
#' frm2 <- tmpl.convert.cols(frm, ctypes)
#' class(frm2$UID)
#' [1] "int64"
#' 
#' }
#' @name tmpl.convert.cols
#' @aliases tmpl.convert.cols
#' @title Convert columns in a data frame.
#' @rdname tmpl.convert.cols
#' @seealso \code{\link{tmpl.cast}}
tmpl.convert.cols <- function(frm, col.types = tmpl.get.option("NZCOL.TYPES", caller = "tmpl.convert.cols"))
{
   if (is.null(col.types) || is.na(col.types) || length(col.types) == 0) return(frm)
   if (!is.list(col.types)) stop("tmpl.convert.cols: expecting a list as the col.types parameter")      
   
   lapply(col.types, function(x) 
                     { 
                        if (is.null(x)) stop("Null entry in NZCOL.TYPES")
                        if (length(x) > 1) stop("Non-scalar entry in NZCOL.TYPES. Expecting a list of scalars")
                     })
   frm.names <- tolower(names(frm))
   col.names <- tolower(names(col.types))
   ix <- match(frm.names, col.names)   
   if (any(!is.na(ix)))
   {
      # found mappings
      ix2 <- which(!is.na(ix))
      for (col in ix2)
      {
         frm[, col] <- tmpl.cast(frm[, col], col.types[[ix[col]]][1])
      }
   }
   return(frm)
}

#-------------------------------------------------------------------------------

#' \code{tmpl.collapse.df} Collapses a data.frame for printing in an e-mail in the
#' form of KEY: value list, where KEY is the column name and value is the value of 
#' the column in the row.
#' @param df Data frame to be collapsed.
#' @param first.nrows Print only the first N rows.
#' @param rec.sep Record separator.
#' @examples 
#' \dontrun{
#' df <- data.frame(name = c("Alex", "Chris", "Dana"), 
#'    phone = c("x1234", "x3456", "x8976"), stringsAsFactors=F)
#' df
#' #    name phone
#' # 1  Alex x1234
#' # 2 Chris x3456
#' # 3  Dana x8976
#' 
#' tmpl.collapse.df(df)
#' # [1] "name: Alex phone: x1234\nname: Chris phone: x3456\nname: Dana phone: x8976"
#' 
#' writeLines(tmpl.collapse.df(df, rec.sep = " | "))
#' # name: Alex | phone: x1234
#' # name: Chris | phone: x3456
#' # name: Dana | phone: x8976
#' }
#' @name tmpl.collapse.df
#' @aliases tmpl.collapse.df
#' @title Format a data frame as key-value pairs.
#' @rdname tmpl.collapse.df
tmpl.collapse.df <- function(df, first.nrows = Inf, rec.sep = " ") 
{
   N <- nrow(df)
   # Subset to just first rows
   df <- subset(df, 1:N <= first.nrows)
   # Collapse
   paste(sapply(1:N, function(i) paste(paste(names(df), df[i, ], sep = ": "), collapse = rec.sep)), collapse = "\n")
}

#-------------------------------------------------------------------------------

#' Generates a formatted text representation of a data frame.
#' @param df Data frame.
#' @param header.line Flag indicating whether to add a line between the header and the data.
#' @param header Flag indicating whether to add a header.
#' @param transpose Flag indicating whether to transpose the data frame
#' @examples
#' \dontrun{
#' writeLines(tmpl.format.df(tmpl.nzquery(
#'    "select * from aud_optim..input_master order by run_id desc limit 1", 
#'    dsn = "NZDS_ERNIE"), transpose = T))
#' }
#' @name tmpl.format.df 
#' @aliases tmpl.format.df 
#' @title Create a formatted data frame string.
#' @rdname tmpl.format.df
tmpl.format.df <- function(df, header.line = TRUE, header = TRUE, transpose = FALSE) 
{   
   # Construct a matrix
   m <- as.matrix(format(df))
   
   # Add titles
   if (header) m <- rbind(matrix(names(df), nrow = 1), m)
   
   # Transpose if necessary
   if (transpose) m <- t(m)
   
   # Get spaces.  
   max.nchar <- apply(m, 2, nchar)
   if (!is.null(dim(max.nchar))) 
      max.nchar <- apply(max.nchar, 2, max)
   
   # Add spaces for fixed width
   for (j in 1:ncol(m)) 
   {
      col <- m[, j, drop = FALSE]
      for (i in 1:length(col))
         col[i] <- paste(col[i], paste(rep(" ", max.nchar[j] - nchar(col[i])), collapse = ""))
      m[, j] <- col
   }
   
   # Add header line if needed
   if (header.line) 
   {
      hline <- sapply(max.nchar, function(n) paste(paste(rep("-", n), collapse = ""), " ", sep = ""))
      hline <- matrix(hline, nrow = 1)
      m <- rbind(m[1, , drop = FALSE], hline, m[-1, , drop = FALSE])
   }
   
   # Collapse
   f <- paste(apply(m, 1, paste, collapse = ""), collapse = "\n")
   
   f   
}

#-------------------------------------------------------------------------------

#' \code{tmpl.double.to.int16} Stores 3 significant digits of \code{a} double in the ranges of
#' -1.0e16 to -1.0e-16 and 1.0e-16 to 1.0e16
#' in two bytes as a signed short integer.
#' @param a A number in double format.
#' @name tmpl.double.to.int16 
#' @aliases tmpl.double.to.int16 
#' @title Functions to convert between double and int2
#' @rdname tmpl.double.to.int16
tmpl.double.to.int16 <- function(a)
{   
   # map zero to zero, and the rest to the number of digits before the dot 
   # so 99.999 -> -2, i.e., we need to multiply by 10^-2 = 100 to get to .xxx format
   m.log <- ifelse(a != 0, floor(-log10(abs(a))), 0)
   
   # The limit is 16 as we only have 5 bits for the log10 and the sign
   if (any(abs(m.log) > 16)) stop("order of magnitude exceeds 10^16")
   
   # Round the value to a signed integer containing the first 3 significant digits of the number (range -999 to 999);
   # Add 1024 to fit into the first 11 bits;
   # Also add the log10 shifted by 16 to fit in the last 5 bits;
   # Finally subtract 2^15 to fit the result in a signed int16;
   # There's some buffer actually, since the first part is at worst -999
   return(as.integer(round(a * 10^(m.log + 3), 0) + 1024 + (m.log + 16) * 2048) - 32768)
}

#-------------------------------------------------------------------------------

#' \code{tmpl.int16.to.double} Converts the signed short int (16 bits) \code{b} back to a double.
#' @param b A number in signed short integer format.
#' @rdname tmpl.double.to.int16
tmpl.int16.to.double <- function(b) 
{
   uint.b <- b + 32768
   return( ((uint.b %% 2048) - 1024) * 10^(-((floor(uint.b / 2048) - 16) + 3)))
}

#-------------------------------------------------------------------------------
