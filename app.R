#Do File

setwd("C:/Users/Nutzer/Desktop/Skat/skat_evaluation")

library(openxlsx)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DBI)
library(reshape)
library(shinyWidgets)
library(rsconnect)
library(shinydashboard)
library(shinyjs)


jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

