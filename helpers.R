# definitions and helper functions-------------------------------------------------------------
errorFrameColumnNames <- c("table","idField", "idValue","idField2","idValue2","idField3","idValue3","errorVariable",
                           "errorValue","errorType","errorDesc","severity","errorVariable2","errorValue2",
                           "errorTable2","errorVariable3","errorValue3","errorVariable4","errorValue4","errorCode", "PROGRAM")

errorMessageModal <- function(message){
  showModal(modalDialog(easyClose = FALSE,
                        title = "Error",
                        wellPanel(
                          tags$h3(message),
                          tags$br(),
                          tags$h4("Please upload file in correct format")
                        ),
                        fade = FALSE))
}

all_iedea_tables <- c("tblBAS", "tblART", "tblCENTER", "tblDIS", "tblLAB","tblLAB_CD4","tblLAB_RNA","tblLTFU","tblMED","tblPROGRAM","tblVIS")

# addShinyInput: create a character vector of shiny inputs -----------------------------------------------
# argument FUN: input type, like actionButton, checkBoxInput, etc
#         id: first part of id string for Shiny inputs. "button_", "checkBox_" etc
#         index: numeric vector of numbers to be appended to id to create shinyInput id
#         ... other arguments unique to FUN
# adapted from shinyInput function found in many SO posts; looks like Yihui Xie might be the creator

addShinyInput <- function(FUN, id, index, ...) {
  inputs <- character(length(index))
  for (i in index) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

# create colored badges to indicate severity of error/warning
makeBadges <- function(severityLevel){
  # initialize badColor vector to red since most are Errors then replace Warn elements with orange
  badgeColor <- rep("red", length(severityLevel))
  badgeColor[severityLevel == "Warn"] <- "orange"
  paste0('<span class="badge" style="background-color: ', badgeColor,'">',severityLevel,'</span>')
}

# makeDetailsPretty -------------------------------------------------------------------------
# makeDetailsPretty isn't pretty... but it removes empty columns from error detail data frame
# (columns that don't apply to the error selected by the user) *but* is careful to keep
# empty columns when the error IS that the variable is blank
makeDetailsPretty <- function(df, errorVariable){
  #remove empty columns; not every error type has data in each column
  emptyCols <- sapply(df, function(x) all(x=="", na.rm = TRUE))
  #make sure errorValue isn't removed -- the blank entry is important
  emptyCols[["errorValue"]] <- FALSE
  # if the errorVariable2 field is not empty, make sure the errorValue2 column isn't removed
  # blank entries are important if that's the error variable, repeat for errorValues2 and 3
  # I know this isn't elegant!
  if (!emptyCols[["errorVariable2"]]){
    emptyCols[["errorValue2"]] <- FALSE
  }
  if (!emptyCols[["errorVariable3"]]){
    emptyCols[["errorValue3"]] <- FALSE
  }
  if (!emptyCols[["errorVariable4"]]){
    emptyCols[["errorValue4"]] <- FALSE
  }
  
  df = df[!emptyCols]
  numberOfErrors <- nrow(df)
  toShow <- NULL
  
  if (exists("idValue", df)){
    identifier <- df$idField[[1]]
    toShow[[identifier]] <- df$idValue
  }
  if (exists("idValue2", df)){
    identifier <- df$idField2[[1]]
    toShow[[identifier]] <- df$idValue2
  }
  if (exists("idValue3", df)){
    identifier <- df$idField3[[1]]
    toShow[[identifier]] <- df$idValue3
  }
  
  toShow[[errorVariable]] <- df$errorValue
  
  if (length(unique(df$errorDesc)) ==1){
    errorDesc <- paste0("Error description: ",df$errorDesc[[1]])
  }
  else {
    errorDesc <- paste0("See detailed error descriptions in table below")
    toShow[["Details"]] <- df$errorDesc
  }
  # use error variable names as column names for all variables found in df
  if (exists("errorValue2", df)){
    variableName <- df$errorVariable2[[1]]
    toShow[[variableName]] <- df$errorValue2
  }
  if (exists("errorValue3", df)){
    variableName <- df$errorVariable3[[1]]
    toShow[[variableName]] <- df$errorValue3
  }
  if (exists("errorValue4", df)){
    variableName <- df$errorVariable4[[1]]
    toShow[[variableName]] <- df$errorValue4
  }
  if (exists("errorTable2", df)){
    variableName <- df$errorTable2[[1]]
    toShow[[variableName]] <- df$errorTable2
  }
  
  toShow <- data.frame(as.list(toShow), stringsAsFactors = FALSE)
  return(list(toShow = toShow,
                 errorDesc = errorDesc))
  
}