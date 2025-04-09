---
title: ''
author: ''
date: ''
output: 
  html_document:
    df_print: paged
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    theme: "flatly"
    code_download: TRUE
editor_options: 
  chunk_output_type: console
---











# Survey completion

There are 92 total responses in the survey dataset. Of these, 46 finished the survey (50%). 

Across all 92 respondents, 77 provided consent (83.7%), including 100% of those who completed the survey.

Survey start dates ranged from 2024-05-20 to 2024-08-06. 

**Figure: Survey accrual**

![](trialists_survey_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
<br>
<br>

Survey accrual was similar for those who did vs didn't finish the survey. 

**Figure: Survey progress for those that didn't finish the survey**

![](trialists_survey_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
<br>
<br>

**Figure: Minutes spent on the survey for those that finished**

![](trialists_survey_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
<br>
<br>

**Figure: Minutes spent on the survey for those that didn't finish**

![](trialists_survey_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
<br>
<br>

**Figure: Number of non-missing values across all survey variables for finishers**

![](trialists_survey_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
<br>
<br>

**Figure: Number of non-missing values across all survey variables for non-finishers**

![](trialists_survey_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
<br>
<br>

Finishers vs non-finishers on Q1-Q7


```{=html}
<div id="bletnlfrii" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bletnlfrii table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bletnlfrii thead, #bletnlfrii tbody, #bletnlfrii tfoot, #bletnlfrii tr, #bletnlfrii td, #bletnlfrii th {
  border-style: none;
}

#bletnlfrii p {
  margin: 0;
  padding: 0;
}

#bletnlfrii .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bletnlfrii .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bletnlfrii .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bletnlfrii .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bletnlfrii .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bletnlfrii .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bletnlfrii .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bletnlfrii .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bletnlfrii .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bletnlfrii .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bletnlfrii .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bletnlfrii .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bletnlfrii .gt_spanner_row {
  border-bottom-style: hidden;
}

#bletnlfrii .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#bletnlfrii .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bletnlfrii .gt_from_md > :first-child {
  margin-top: 0;
}

#bletnlfrii .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bletnlfrii .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bletnlfrii .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#bletnlfrii .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#bletnlfrii .gt_row_group_first td {
  border-top-width: 2px;
}

#bletnlfrii .gt_row_group_first th {
  border-top-width: 2px;
}

#bletnlfrii .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bletnlfrii .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bletnlfrii .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bletnlfrii .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bletnlfrii .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bletnlfrii .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bletnlfrii .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bletnlfrii .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bletnlfrii .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bletnlfrii .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bletnlfrii .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bletnlfrii .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bletnlfrii .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bletnlfrii .gt_left {
  text-align: left;
}

#bletnlfrii .gt_center {
  text-align: center;
}

#bletnlfrii .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bletnlfrii .gt_font_normal {
  font-weight: normal;
}

#bletnlfrii .gt_font_bold {
  font-weight: bold;
}

#bletnlfrii .gt_font_italic {
  font-style: italic;
}

#bletnlfrii .gt_super {
  font-size: 65%;
}

#bletnlfrii .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bletnlfrii .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bletnlfrii .gt_indent_1 {
  text-indent: 5px;
}

#bletnlfrii .gt_indent_2 {
  text-indent: 10px;
}

#bletnlfrii .gt_indent_3 {
  text-indent: 15px;
}

#bletnlfrii .gt_indent_4 {
  text-indent: 20px;
}

#bletnlfrii .gt_indent_5 {
  text-indent: 25px;
}

#bletnlfrii .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bletnlfrii div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="**Finished survey?**">
        <div class="gt_column_spanner"><span class='gt_from_md'><strong>Finished survey?</strong></span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>No</strong><br />
N = 46</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Yes</strong><br />
N = 46</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">What is your current career stage?</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Undergraduate/ Masters student</td>
<td headers="stat_1" class="gt_row gt_center">0/12 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">1/46 (2.2%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Doctoral student</td>
<td headers="stat_1" class="gt_row gt_center">0/12 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">3/46 (6.5%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Early-career researcher (&lt; 7 years post PhD)</td>
<td headers="stat_1" class="gt_row gt_center">0/12 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">8/46 (17%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mid-career researcher (7-12 years post PhD)</td>
<td headers="stat_1" class="gt_row gt_center">4/12 (33%)</td>
<td headers="stat_2" class="gt_row gt_center">6/46 (13%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Senior-career researcher (&gt;12 years post PhD)</td>
<td headers="stat_1" class="gt_row gt_center">8/12 (67%)</td>
<td headers="stat_2" class="gt_row gt_center">28/46 (61%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">How many years of research experience do you have in childhood obesity prevention trials?\n\n(please enter numeric values only)</td>
<td headers="stat_1" class="gt_row gt_center">10 (7, 15)</td>
<td headers="stat_2" class="gt_row gt_center">10 (5, 20)</td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Have you undertaken training in trial methods?</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">1/11 (9.1%)</td>
<td headers="stat_2" class="gt_row gt_center">10/46 (22%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_1" class="gt_row gt_center">10/11 (91%)</td>
<td headers="stat_2" class="gt_row gt_center">36/46 (78%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">How many trials of interventions to prevent obesity, and/or to improve child health behaviours related to childhood obesity, in children from 0 to 5 years old have you been involved in to date?</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">0.2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="stat_1" class="gt_row gt_center">2/11 (18%)</td>
<td headers="stat_2" class="gt_row gt_center">1/46 (2.2%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_1" class="gt_row gt_center">2/11 (18%)</td>
<td headers="stat_2" class="gt_row gt_center">11/46 (24%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_1" class="gt_row gt_center">2/11 (18%)</td>
<td headers="stat_2" class="gt_row gt_center">11/46 (24%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_1" class="gt_row gt_center">1/11 (9.1%)</td>
<td headers="stat_2" class="gt_row gt_center">10/46 (22%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    4</td>
<td headers="stat_1" class="gt_row gt_center">3/11 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">2/46 (4.3%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    5</td>
<td headers="stat_1" class="gt_row gt_center">0/11 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">4/46 (8.7%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    6</td>
<td headers="stat_1" class="gt_row gt_center">0/11 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">2/46 (4.3%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7</td>
<td headers="stat_1" class="gt_row gt_center">0/11 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">1/46 (2.2%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    8</td>
<td headers="stat_1" class="gt_row gt_center">1/11 (9.1%)</td>
<td headers="stat_2" class="gt_row gt_center">2/46 (4.3%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    more than 10</td>
<td headers="stat_1" class="gt_row gt_center">0/11 (0%)</td>
<td headers="stat_2" class="gt_row gt_center">2/46 (4.3%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_1" class="gt_row gt_center">35</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Are you currently involved in a childhood obesity prevention trial/s?</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center">>0.9</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_1" class="gt_row gt_center">5/12 (42%)</td>
<td headers="stat_2" class="gt_row gt_center">20/46 (43%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes, I am currently involved in 1 trial</td>
<td headers="stat_1" class="gt_row gt_center">4/12 (33%)</td>
<td headers="stat_2" class="gt_row gt_center">14/46 (30%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes, I am currently involved in more than 1 trial</td>
<td headers="stat_1" class="gt_row gt_center">3/12 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">12/46 (26%)</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_1" class="gt_row gt_center">34</td>
<td headers="stat_2" class="gt_row gt_center">0</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%); Median (Q1, Q3)</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>Pearson’s Chi-squared test; Wilcoxon rank sum test</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

List of Countries
 
![](trialists_survey_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


In what country/countries were the trials in which you are/were involved conducted?
 
![](trialists_survey_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

# Results

## (Q1-7) Repondent characteristics

From this point forward the dataset it limited to the 46 participants who completed the survey. 



#### Table: Respondent characteristics


```{=html}
<div id="uduthbqqtc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uduthbqqtc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#uduthbqqtc thead, #uduthbqqtc tbody, #uduthbqqtc tfoot, #uduthbqqtc tr, #uduthbqqtc td, #uduthbqqtc th {
  border-style: none;
}

#uduthbqqtc p {
  margin: 0;
  padding: 0;
}

#uduthbqqtc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uduthbqqtc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#uduthbqqtc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uduthbqqtc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uduthbqqtc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uduthbqqtc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uduthbqqtc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uduthbqqtc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uduthbqqtc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uduthbqqtc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uduthbqqtc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uduthbqqtc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uduthbqqtc .gt_spanner_row {
  border-bottom-style: hidden;
}

#uduthbqqtc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#uduthbqqtc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uduthbqqtc .gt_from_md > :first-child {
  margin-top: 0;
}

#uduthbqqtc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uduthbqqtc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uduthbqqtc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#uduthbqqtc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#uduthbqqtc .gt_row_group_first td {
  border-top-width: 2px;
}

#uduthbqqtc .gt_row_group_first th {
  border-top-width: 2px;
}

#uduthbqqtc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uduthbqqtc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uduthbqqtc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uduthbqqtc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uduthbqqtc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uduthbqqtc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uduthbqqtc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#uduthbqqtc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uduthbqqtc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uduthbqqtc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uduthbqqtc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uduthbqqtc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uduthbqqtc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uduthbqqtc .gt_left {
  text-align: left;
}

#uduthbqqtc .gt_center {
  text-align: center;
}

#uduthbqqtc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uduthbqqtc .gt_font_normal {
  font-weight: normal;
}

#uduthbqqtc .gt_font_bold {
  font-weight: bold;
}

#uduthbqqtc .gt_font_italic {
  font-style: italic;
}

#uduthbqqtc .gt_super {
  font-size: 65%;
}

#uduthbqqtc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#uduthbqqtc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uduthbqqtc .gt_indent_1 {
  text-indent: 5px;
}

#uduthbqqtc .gt_indent_2 {
  text-indent: 10px;
}

#uduthbqqtc .gt_indent_3 {
  text-indent: 15px;
}

#uduthbqqtc .gt_indent_4 {
  text-indent: 20px;
}

#uduthbqqtc .gt_indent_5 {
  text-indent: 25px;
}

#uduthbqqtc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#uduthbqqtc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 46</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">What is your current career stage?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Undergraduate/ Masters student</td>
<td headers="stat_0" class="gt_row gt_center">1/46 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Doctoral student</td>
<td headers="stat_0" class="gt_row gt_center">3/46 (6.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Early-career researcher (&lt; 7 years post PhD)</td>
<td headers="stat_0" class="gt_row gt_center">8/46 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mid-career researcher (7-12 years post PhD)</td>
<td headers="stat_0" class="gt_row gt_center">6/46 (13%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Senior-career researcher (&gt;12 years post PhD)</td>
<td headers="stat_0" class="gt_row gt_center">28/46 (61%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">How many years of research experience do you have in childhood obesity prevention trials?\n\n(please enter numeric values only)</td>
<td headers="stat_0" class="gt_row gt_center">10 (5, 20)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Have you undertaken training in trial methods?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">10/46 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes</td>
<td headers="stat_0" class="gt_row gt_center">36/46 (78%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">How many trials of interventions to prevent obesity, and/or to improve child health behaviours related to childhood obesity, in children from 0 to 5 years old have you been involved in to date?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="stat_0" class="gt_row gt_center">1/46 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">11/46 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">11/46 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">10/46 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    4</td>
<td headers="stat_0" class="gt_row gt_center">2/46 (4.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    5</td>
<td headers="stat_0" class="gt_row gt_center">4/46 (8.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    6</td>
<td headers="stat_0" class="gt_row gt_center">2/46 (4.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7</td>
<td headers="stat_0" class="gt_row gt_center">1/46 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    8</td>
<td headers="stat_0" class="gt_row gt_center">2/46 (4.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    more than 10</td>
<td headers="stat_0" class="gt_row gt_center">2/46 (4.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Are you currently involved in a childhood obesity prevention trial/s?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No</td>
<td headers="stat_0" class="gt_row gt_center">20/46 (43%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes, I am currently involved in 1 trial</td>
<td headers="stat_0" class="gt_row gt_center">14/46 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes, I am currently involved in more than 1 trial</td>
<td headers="stat_0" class="gt_row gt_center">12/46 (26%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%); Median (Q1, Q3)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

#### Figure: List of Countries (Q1)
 
![](trialists_survey_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

#### Table: List of Countries (Q1)


```{=html}
<div id="slgwdwodiw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#slgwdwodiw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#slgwdwodiw thead, #slgwdwodiw tbody, #slgwdwodiw tfoot, #slgwdwodiw tr, #slgwdwodiw td, #slgwdwodiw th {
  border-style: none;
}

#slgwdwodiw p {
  margin: 0;
  padding: 0;
}

#slgwdwodiw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#slgwdwodiw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#slgwdwodiw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#slgwdwodiw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#slgwdwodiw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#slgwdwodiw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#slgwdwodiw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#slgwdwodiw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#slgwdwodiw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#slgwdwodiw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#slgwdwodiw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#slgwdwodiw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#slgwdwodiw .gt_spanner_row {
  border-bottom-style: hidden;
}

#slgwdwodiw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#slgwdwodiw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#slgwdwodiw .gt_from_md > :first-child {
  margin-top: 0;
}

#slgwdwodiw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#slgwdwodiw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#slgwdwodiw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#slgwdwodiw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#slgwdwodiw .gt_row_group_first td {
  border-top-width: 2px;
}

#slgwdwodiw .gt_row_group_first th {
  border-top-width: 2px;
}

#slgwdwodiw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#slgwdwodiw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#slgwdwodiw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#slgwdwodiw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#slgwdwodiw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#slgwdwodiw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#slgwdwodiw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#slgwdwodiw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#slgwdwodiw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#slgwdwodiw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#slgwdwodiw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#slgwdwodiw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#slgwdwodiw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#slgwdwodiw .gt_left {
  text-align: left;
}

#slgwdwodiw .gt_center {
  text-align: center;
}

#slgwdwodiw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#slgwdwodiw .gt_font_normal {
  font-weight: normal;
}

#slgwdwodiw .gt_font_bold {
  font-weight: bold;
}

#slgwdwodiw .gt_font_italic {
  font-style: italic;
}

#slgwdwodiw .gt_super {
  font-size: 65%;
}

#slgwdwodiw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#slgwdwodiw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#slgwdwodiw .gt_indent_1 {
  text-indent: 5px;
}

#slgwdwodiw .gt_indent_2 {
  text-indent: 10px;
}

#slgwdwodiw .gt_indent_3 {
  text-indent: 15px;
}

#slgwdwodiw .gt_indent_4 {
  text-indent: 20px;
}

#slgwdwodiw .gt_indent_5 {
  text-indent: 25px;
}

#slgwdwodiw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#slgwdwodiw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 14</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">q1</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Brazil</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    India</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    New Zealand</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Norway</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Spain</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sri Lanka</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Canada</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    France</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sweden</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    United Kingdom of Great Britain and Northern Ireland</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Ireland</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Australia</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    United States of America</td>
<td headers="stat_0" class="gt_row gt_center">1/13 (7.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_0" class="gt_row gt_center">1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">q1_prop</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0.0217391304347826</td>
<td headers="stat_0" class="gt_row gt_center">7/14 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0.0434782608695652</td>
<td headers="stat_0" class="gt_row gt_center">2/14 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0.108695652173913</td>
<td headers="stat_0" class="gt_row gt_center">2/14 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0.130434782608696</td>
<td headers="stat_0" class="gt_row gt_center">1/14 (7.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0.173913043478261</td>
<td headers="stat_0" class="gt_row gt_center">1/14 (7.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0.239130434782609</td>
<td headers="stat_0" class="gt_row gt_center">1/14 (7.1%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

#### Figure: What is your current career stage? (Q2)

![](trialists_survey_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

#### Figure: How many trials of interventions to prevent obesity, and/or to improve child health behaviours related to childhood obesity, in children from 0 to 5 years old have you been involved in to date? (Q5)

![](trialists_survey_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

#### Figure: In what country/countries were the trials in which you are/were involved conducted? (Q6)
 
![](trialists_survey_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

#### Table: In what country/countries were the trials in which you are/were involved conducted? (Q6)


```{=html}
<div id="qrfecegzea" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qrfecegzea table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#qrfecegzea thead, #qrfecegzea tbody, #qrfecegzea tfoot, #qrfecegzea tr, #qrfecegzea td, #qrfecegzea th {
  border-style: none;
}

#qrfecegzea p {
  margin: 0;
  padding: 0;
}

#qrfecegzea .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qrfecegzea .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#qrfecegzea .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qrfecegzea .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qrfecegzea .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qrfecegzea .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrfecegzea .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qrfecegzea .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qrfecegzea .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qrfecegzea .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qrfecegzea .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qrfecegzea .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qrfecegzea .gt_spanner_row {
  border-bottom-style: hidden;
}

#qrfecegzea .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#qrfecegzea .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qrfecegzea .gt_from_md > :first-child {
  margin-top: 0;
}

#qrfecegzea .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qrfecegzea .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qrfecegzea .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#qrfecegzea .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#qrfecegzea .gt_row_group_first td {
  border-top-width: 2px;
}

#qrfecegzea .gt_row_group_first th {
  border-top-width: 2px;
}

#qrfecegzea .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrfecegzea .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#qrfecegzea .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#qrfecegzea .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrfecegzea .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrfecegzea .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qrfecegzea .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#qrfecegzea .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qrfecegzea .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrfecegzea .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qrfecegzea .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrfecegzea .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qrfecegzea .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrfecegzea .gt_left {
  text-align: left;
}

#qrfecegzea .gt_center {
  text-align: center;
}

#qrfecegzea .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qrfecegzea .gt_font_normal {
  font-weight: normal;
}

#qrfecegzea .gt_font_bold {
  font-weight: bold;
}

#qrfecegzea .gt_font_italic {
  font-style: italic;
}

#qrfecegzea .gt_super {
  font-size: 65%;
}

#qrfecegzea .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#qrfecegzea .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#qrfecegzea .gt_indent_1 {
  text-indent: 5px;
}

#qrfecegzea .gt_indent_2 {
  text-indent: 10px;
}

#qrfecegzea .gt_indent_3 {
  text-indent: 15px;
}

#qrfecegzea .gt_indent_4 {
  text-indent: 20px;
}

#qrfecegzea .gt_indent_5 {
  text-indent: 25px;
}

#qrfecegzea .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#qrfecegzea div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 60</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">country</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Brazil</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Canada</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    China</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Denmark</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    India</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    KSA</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    New Zealand</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Puerto Rico</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    South Africa</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sri Lanka</td>
<td headers="stat_0" class="gt_row gt_center">1/60 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Spain</td>
<td headers="stat_0" class="gt_row gt_center">2/60 (3.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    France</td>
<td headers="stat_0" class="gt_row gt_center">3/60 (5.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sweden</td>
<td headers="stat_0" class="gt_row gt_center">5/60 (8.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Ireland</td>
<td headers="stat_0" class="gt_row gt_center">6/60 (10%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    UK</td>
<td headers="stat_0" class="gt_row gt_center">8/60 (13%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Australia</td>
<td headers="stat_0" class="gt_row gt_center">13/60 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    USA</td>
<td headers="stat_0" class="gt_row gt_center">13/60 (22%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

## (Q8) Trial Status

#### Figure: Number of current trials for each respondent

![](trialists_survey_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

#### Figure: Status of all current trials

![](trialists_survey_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

#### Figure: Number and status of all current trials, by respondent

![](trialists_survey_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

#### Table: At what stage is your current trial(s)?\n\nPlease select one option for each trial you are currently involved in - Trial 1-4


```{=html}
<div id="kyfcmcrige" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#kyfcmcrige table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#kyfcmcrige thead, #kyfcmcrige tbody, #kyfcmcrige tfoot, #kyfcmcrige tr, #kyfcmcrige td, #kyfcmcrige th {
  border-style: none;
}

#kyfcmcrige p {
  margin: 0;
  padding: 0;
}

#kyfcmcrige .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kyfcmcrige .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#kyfcmcrige .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kyfcmcrige .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kyfcmcrige .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kyfcmcrige .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kyfcmcrige .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kyfcmcrige .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kyfcmcrige .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kyfcmcrige .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kyfcmcrige .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kyfcmcrige .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kyfcmcrige .gt_spanner_row {
  border-bottom-style: hidden;
}

#kyfcmcrige .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#kyfcmcrige .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kyfcmcrige .gt_from_md > :first-child {
  margin-top: 0;
}

#kyfcmcrige .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kyfcmcrige .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kyfcmcrige .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#kyfcmcrige .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#kyfcmcrige .gt_row_group_first td {
  border-top-width: 2px;
}

#kyfcmcrige .gt_row_group_first th {
  border-top-width: 2px;
}

#kyfcmcrige .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kyfcmcrige .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#kyfcmcrige .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#kyfcmcrige .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kyfcmcrige .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kyfcmcrige .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kyfcmcrige .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#kyfcmcrige .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kyfcmcrige .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kyfcmcrige .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kyfcmcrige .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kyfcmcrige .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kyfcmcrige .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kyfcmcrige .gt_left {
  text-align: left;
}

#kyfcmcrige .gt_center {
  text-align: center;
}

#kyfcmcrige .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kyfcmcrige .gt_font_normal {
  font-weight: normal;
}

#kyfcmcrige .gt_font_bold {
  font-weight: bold;
}

#kyfcmcrige .gt_font_italic {
  font-style: italic;
}

#kyfcmcrige .gt_super {
  font-size: 65%;
}

#kyfcmcrige .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#kyfcmcrige .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#kyfcmcrige .gt_indent_1 {
  text-indent: 5px;
}

#kyfcmcrige .gt_indent_2 {
  text-indent: 10px;
}

#kyfcmcrige .gt_indent_3 {
  text-indent: 15px;
}

#kyfcmcrige .gt_indent_4 {
  text-indent: 20px;
}

#kyfcmcrige .gt_indent_5 {
  text-indent: 25px;
}

#kyfcmcrige .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#kyfcmcrige div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 46</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">At what stage is your current trial(s)?\n\nPlease select one option for each trial you are currently involved in - Trial 1</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Planned (recruitment not started)</td>
<td headers="stat_0" class="gt_row gt_center">5/26 (19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Ongoing</td>
<td headers="stat_0" class="gt_row gt_center">9/26 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Data collection completed (but not analysed)</td>
<td headers="stat_0" class="gt_row gt_center">7/26 (27%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Reporting/published</td>
<td headers="stat_0" class="gt_row gt_center">5/26 (19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_0" class="gt_row gt_center">20</td></tr>
    <tr><td headers="label" class="gt_row gt_left">At what stage is your current trial(s)?\n\nPlease select one option for each trial you are currently involved in - Trial 2</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Planned (recruitment not started)</td>
<td headers="stat_0" class="gt_row gt_center">3/15 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Ongoing</td>
<td headers="stat_0" class="gt_row gt_center">5/15 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Data collection completed (but not analysed)</td>
<td headers="stat_0" class="gt_row gt_center">0/15 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Reporting/published</td>
<td headers="stat_0" class="gt_row gt_center">7/15 (47%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_0" class="gt_row gt_center">31</td></tr>
    <tr><td headers="label" class="gt_row gt_left">At what stage is your current trial(s)?\n\nPlease select one option for each trial you are currently involved in - Trial 3</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Planned (recruitment not started)</td>
<td headers="stat_0" class="gt_row gt_center">0/6 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Ongoing</td>
<td headers="stat_0" class="gt_row gt_center">3/6 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Data collection completed (but not analysed)</td>
<td headers="stat_0" class="gt_row gt_center">1/6 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Reporting/published</td>
<td headers="stat_0" class="gt_row gt_center">2/6 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_0" class="gt_row gt_center">40</td></tr>
    <tr><td headers="label" class="gt_row gt_left">At what stage is your current trial(s)?\n\nPlease select one option for each trial you are currently involved in - Trial 4</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Planned (recruitment not started)</td>
<td headers="stat_0" class="gt_row gt_center">0/4 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Ongoing</td>
<td headers="stat_0" class="gt_row gt_center">3/4 (75%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Data collection completed (but not analysed)</td>
<td headers="stat_0" class="gt_row gt_center">1/4 (25%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Reporting/published</td>
<td headers="stat_0" class="gt_row gt_center">0/4 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_0" class="gt_row gt_center">42</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```



## (Q9) Current and previous roles working on childhood obesity prevention trials 

#### Figure: Current and previous roles working on childhood obesity prevention trials across all respondents

![](trialists_survey_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

#### Free text


```
## [1] "Database manager; PhD student; We submitted a proposal to get funding for a childhood obesity trial"
```

## (Q10) Methods for identifying outcomes 

#### Figure: Methods for identifying outcomes as reported across all respondents

![](trialists_survey_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

#### Free text


```
## [1] "WHO ICF Core Outcome Sets; Innovative ideas from teams; Outcomes come from previous trials (under this body of work) - based on the protocol of this trial, it is possible to understand what the outcomes would come from but I couldn't identify them off the top of my head.; Causal logic model"
```

## (Q11 & 12) Familiarity with core outcome sets 

Among the 46 respondents, 32 indicated they had heard of, or were familiar with, core outcome sets prior to taking the survey. 

#### Figure: Sources of previous exposure to core outcome sets

![](trialists_survey_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

Note: I don't see an actual open text variable associated with this set of responses. 

## (Q13) Familarity with existing core outcomes sets for childhood obesity trials 

#### Table: Familarity with existing core outcomes sets for childhood obesity trials


```{=html}
<div id="gmxwgbtfwx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gmxwgbtfwx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gmxwgbtfwx thead, #gmxwgbtfwx tbody, #gmxwgbtfwx tfoot, #gmxwgbtfwx tr, #gmxwgbtfwx td, #gmxwgbtfwx th {
  border-style: none;
}

#gmxwgbtfwx p {
  margin: 0;
  padding: 0;
}

#gmxwgbtfwx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gmxwgbtfwx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gmxwgbtfwx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gmxwgbtfwx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gmxwgbtfwx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gmxwgbtfwx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gmxwgbtfwx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gmxwgbtfwx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gmxwgbtfwx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gmxwgbtfwx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gmxwgbtfwx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gmxwgbtfwx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gmxwgbtfwx .gt_spanner_row {
  border-bottom-style: hidden;
}

#gmxwgbtfwx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gmxwgbtfwx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gmxwgbtfwx .gt_from_md > :first-child {
  margin-top: 0;
}

#gmxwgbtfwx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gmxwgbtfwx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gmxwgbtfwx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#gmxwgbtfwx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gmxwgbtfwx .gt_row_group_first td {
  border-top-width: 2px;
}

#gmxwgbtfwx .gt_row_group_first th {
  border-top-width: 2px;
}

#gmxwgbtfwx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gmxwgbtfwx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gmxwgbtfwx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gmxwgbtfwx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gmxwgbtfwx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gmxwgbtfwx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gmxwgbtfwx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gmxwgbtfwx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gmxwgbtfwx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gmxwgbtfwx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gmxwgbtfwx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gmxwgbtfwx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gmxwgbtfwx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gmxwgbtfwx .gt_left {
  text-align: left;
}

#gmxwgbtfwx .gt_center {
  text-align: center;
}

#gmxwgbtfwx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gmxwgbtfwx .gt_font_normal {
  font-weight: normal;
}

#gmxwgbtfwx .gt_font_bold {
  font-weight: bold;
}

#gmxwgbtfwx .gt_font_italic {
  font-style: italic;
}

#gmxwgbtfwx .gt_super {
  font-size: 65%;
}

#gmxwgbtfwx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gmxwgbtfwx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gmxwgbtfwx .gt_indent_1 {
  text-indent: 5px;
}

#gmxwgbtfwx .gt_indent_2 {
  text-indent: 10px;
}

#gmxwgbtfwx .gt_indent_3 {
  text-indent: 15px;
}

#gmxwgbtfwx .gt_indent_4 {
  text-indent: 20px;
}

#gmxwgbtfwx .gt_indent_5 {
  text-indent: 25px;
}

#gmxwgbtfwx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#gmxwgbtfwx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 46</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Are you familiar with these core outcome sets?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No, I am not familiar with either core outcome set</td>
<td headers="stat_0" class="gt_row gt_center">5/31 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes, I am familiar with both core outcome sets</td>
<td headers="stat_0" class="gt_row gt_center">20/31 (65%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Yes, I am familiar with the EPOCH core outcome set for obesity prevention up to 5 years only</td>
<td headers="stat_0" class="gt_row gt_center">6/31 (19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing values</td>
<td headers="stat_0" class="gt_row gt_center">15</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

## (Q14) Sources of familiarity with existing core outcomes sets for childhood obesity 

#### Figure: Sources of previous exposure to core outcome sets

![](trialists_survey_files/figure-html/unnamed-chunk-33-1.png)<!-- -->


```{=html}
<div id="qckszfvllv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qckszfvllv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#qckszfvllv thead, #qckszfvllv tbody, #qckszfvllv tfoot, #qckszfvllv tr, #qckszfvllv td, #qckszfvllv th {
  border-style: none;
}

#qckszfvllv p {
  margin: 0;
  padding: 0;
}

#qckszfvllv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qckszfvllv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#qckszfvllv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qckszfvllv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qckszfvllv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qckszfvllv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qckszfvllv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qckszfvllv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qckszfvllv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qckszfvllv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qckszfvllv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qckszfvllv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qckszfvllv .gt_spanner_row {
  border-bottom-style: hidden;
}

#qckszfvllv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#qckszfvllv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qckszfvllv .gt_from_md > :first-child {
  margin-top: 0;
}

#qckszfvllv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qckszfvllv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qckszfvllv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#qckszfvllv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#qckszfvllv .gt_row_group_first td {
  border-top-width: 2px;
}

#qckszfvllv .gt_row_group_first th {
  border-top-width: 2px;
}

#qckszfvllv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qckszfvllv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#qckszfvllv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#qckszfvllv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qckszfvllv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qckszfvllv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qckszfvllv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#qckszfvllv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qckszfvllv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qckszfvllv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qckszfvllv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qckszfvllv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qckszfvllv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#qckszfvllv .gt_left {
  text-align: left;
}

#qckszfvllv .gt_center {
  text-align: center;
}

#qckszfvllv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qckszfvllv .gt_font_normal {
  font-weight: normal;
}

#qckszfvllv .gt_font_bold {
  font-weight: bold;
}

#qckszfvllv .gt_font_italic {
  font-style: italic;
}

#qckszfvllv .gt_super {
  font-size: 65%;
}

#qckszfvllv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#qckszfvllv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#qckszfvllv .gt_indent_1 {
  text-indent: 5px;
}

#qckszfvllv .gt_indent_2 {
  text-indent: 10px;
}

#qckszfvllv .gt_indent_3 {
  text-indent: 15px;
}

#qckszfvllv .gt_indent_4 {
  text-indent: 20px;
}

#qckszfvllv .gt_indent_5 {
  text-indent: 25px;
}

#qckszfvllv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#qckszfvllv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n"><span class='gt_from_md'><strong>N</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>EPOCH COS up to 5 years</strong><br />
N = 55</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Infant feeding COS</strong><br />
N = 57</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Source</td>
<td headers="n" class="gt_row gt_center">112</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other, please specify</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">1/55 (1.8%)</td>
<td headers="stat_2" class="gt_row gt_center">1/57 (1.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I worked in the development of a COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">2/55 (3.6%)</td>
<td headers="stat_2" class="gt_row gt_center">3/57 (5.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I have seen this COS reported in a trial</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">4/55 (7.3%)</td>
<td headers="stat_2" class="gt_row gt_center">5/57 (8.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    A colleague has told me about this COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">6/55 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">5/57 (8.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I was involved as a participant in a Delphi or consensus process to develop a COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">3/55 (5.5%)</td>
<td headers="stat_2" class="gt_row gt_center">8/57 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I have attended a conference presentation/seminar/talk on this COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">6/55 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">7/57 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I have seen this COS reported/discussed in another type of research</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">8/55 (15%)</td>
<td headers="stat_2" class="gt_row gt_center">6/57 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I have used this COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">9/55 (16%)</td>
<td headers="stat_2" class="gt_row gt_center">8/57 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    I have read the publication(s) on this COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">16/55 (29%)</td>
<td headers="stat_2" class="gt_row gt_center">14/57 (25%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>112 sources endorsed by 25 respondents</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

Note: No actual open text field for "other". 

## (Q15) Barriers to using childhood obesity prevention core outcome set(s) in a trial 

#### Figure: Barriers to using childhood obesity prevention core outcome set(s) in a trial

![](trialists_survey_files/figure-html/unnamed-chunk-35-1.png)<!-- -->


```{=html}
<div id="ctuhxnlsvw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ctuhxnlsvw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ctuhxnlsvw thead, #ctuhxnlsvw tbody, #ctuhxnlsvw tfoot, #ctuhxnlsvw tr, #ctuhxnlsvw td, #ctuhxnlsvw th {
  border-style: none;
}

#ctuhxnlsvw p {
  margin: 0;
  padding: 0;
}

#ctuhxnlsvw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ctuhxnlsvw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ctuhxnlsvw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ctuhxnlsvw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ctuhxnlsvw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ctuhxnlsvw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ctuhxnlsvw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ctuhxnlsvw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ctuhxnlsvw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ctuhxnlsvw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ctuhxnlsvw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ctuhxnlsvw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ctuhxnlsvw .gt_spanner_row {
  border-bottom-style: hidden;
}

#ctuhxnlsvw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ctuhxnlsvw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ctuhxnlsvw .gt_from_md > :first-child {
  margin-top: 0;
}

#ctuhxnlsvw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ctuhxnlsvw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ctuhxnlsvw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ctuhxnlsvw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ctuhxnlsvw .gt_row_group_first td {
  border-top-width: 2px;
}

#ctuhxnlsvw .gt_row_group_first th {
  border-top-width: 2px;
}

#ctuhxnlsvw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ctuhxnlsvw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ctuhxnlsvw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ctuhxnlsvw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ctuhxnlsvw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ctuhxnlsvw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ctuhxnlsvw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ctuhxnlsvw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ctuhxnlsvw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ctuhxnlsvw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ctuhxnlsvw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ctuhxnlsvw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ctuhxnlsvw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ctuhxnlsvw .gt_left {
  text-align: left;
}

#ctuhxnlsvw .gt_center {
  text-align: center;
}

#ctuhxnlsvw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ctuhxnlsvw .gt_font_normal {
  font-weight: normal;
}

#ctuhxnlsvw .gt_font_bold {
  font-weight: bold;
}

#ctuhxnlsvw .gt_font_italic {
  font-style: italic;
}

#ctuhxnlsvw .gt_super {
  font-size: 65%;
}

#ctuhxnlsvw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ctuhxnlsvw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ctuhxnlsvw .gt_indent_1 {
  text-indent: 5px;
}

#ctuhxnlsvw .gt_indent_2 {
  text-indent: 10px;
}

#ctuhxnlsvw .gt_indent_3 {
  text-indent: 15px;
}

#ctuhxnlsvw .gt_indent_4 {
  text-indent: 20px;
}

#ctuhxnlsvw .gt_indent_5 {
  text-indent: 25px;
}

#ctuhxnlsvw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ctuhxnlsvw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n"><span class='gt_from_md'><strong>N</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>EPOCH COS up to 5 years</strong><br />
N = 149</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Infant feeding COS</strong><br />
N = 136</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Barrier</td>
<td headers="n" class="gt_row gt_center">285</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not knowing why to use the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">2/149 (1.3%)</td>
<td headers="stat_2" class="gt_row gt_center">0/136 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Trial regulation constraints</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">2/149 (1.3%)</td>
<td headers="stat_2" class="gt_row gt_center">2/136 (1.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other, please specify</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">3/149 (2.0%)</td>
<td headers="stat_2" class="gt_row gt_center">3/136 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not knowing which COS to use</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">3/149 (2.0%)</td>
<td headers="stat_2" class="gt_row gt_center">4/136 (2.9%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Preferring to use outcomes used in other trials</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">5/149 (3.4%)</td>
<td headers="stat_2" class="gt_row gt_center">2/136 (1.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS is not relevant for your geographical location(s)/resource setting(s)</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">5/149 (3.4%)</td>
<td headers="stat_2" class="gt_row gt_center">5/136 (3.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    None</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">7/149 (4.7%)</td>
<td headers="stat_2" class="gt_row gt_center">5/136 (3.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other members of the team not wanting to use the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">7/149 (4.7%)</td>
<td headers="stat_2" class="gt_row gt_center">6/136 (4.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Preferring to use your own chosen outcomes</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">7/149 (4.7%)</td>
<td headers="stat_2" class="gt_row gt_center">6/136 (4.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS restricts what can be measured in a trial</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">8/149 (5.4%)</td>
<td headers="stat_2" class="gt_row gt_center">7/136 (5.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS is not relevant for your population(s) of interest</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">8/149 (5.4%)</td>
<td headers="stat_2" class="gt_row gt_center">9/136 (6.6%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not knowing what the COS is</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">11/149 (7.4%)</td>
<td headers="stat_2" class="gt_row gt_center">10/136 (7.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS is not relevant for the type(s) of intervention you are examining</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">11/149 (7.4%)</td>
<td headers="stat_2" class="gt_row gt_center">11/136 (8.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not knowing how best to measure the outcomes in the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">17/149 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">15/136 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Costs associated with measuring the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">23/149 (15%)</td>
<td headers="stat_2" class="gt_row gt_center">23/136 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Perceived participant burden</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">30/149 (20%)</td>
<td headers="stat_2" class="gt_row gt_center">28/136 (21%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>285 sources endorsed by 44 respondents</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

#### Free text


```
## [1] "Self reported outcomes are unreliable andnot associated with long-term outcome. Physical activity is not associated with weight development in this age group. A large number of excellent studies have failed to affect long-term weight outcome. We need to identify new interventions and outcome parameters based on, maybe based on  \"life history theory\"; Too many outcomes, each trial needs to prioritise; Availability of instruments to measure the core outcome in my langage; number of outcomes and measurement; i am looking for a core outcome set for early child eating behaviours"
```

## (Q16) Faciliators to using childhood obesity prevention core outcome set(s) in a trial 

#### Figure: Faciliators to using childhood obesity prevention core outcome set(s) in a trial

![](trialists_survey_files/figure-html/unnamed-chunk-38-1.png)<!-- -->


```{=html}
<div id="corjwdxnbt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#corjwdxnbt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#corjwdxnbt thead, #corjwdxnbt tbody, #corjwdxnbt tfoot, #corjwdxnbt tr, #corjwdxnbt td, #corjwdxnbt th {
  border-style: none;
}

#corjwdxnbt p {
  margin: 0;
  padding: 0;
}

#corjwdxnbt .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#corjwdxnbt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#corjwdxnbt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#corjwdxnbt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#corjwdxnbt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#corjwdxnbt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#corjwdxnbt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#corjwdxnbt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#corjwdxnbt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#corjwdxnbt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#corjwdxnbt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#corjwdxnbt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#corjwdxnbt .gt_spanner_row {
  border-bottom-style: hidden;
}

#corjwdxnbt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#corjwdxnbt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#corjwdxnbt .gt_from_md > :first-child {
  margin-top: 0;
}

#corjwdxnbt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#corjwdxnbt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#corjwdxnbt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#corjwdxnbt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#corjwdxnbt .gt_row_group_first td {
  border-top-width: 2px;
}

#corjwdxnbt .gt_row_group_first th {
  border-top-width: 2px;
}

#corjwdxnbt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#corjwdxnbt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#corjwdxnbt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#corjwdxnbt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#corjwdxnbt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#corjwdxnbt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#corjwdxnbt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#corjwdxnbt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#corjwdxnbt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#corjwdxnbt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#corjwdxnbt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#corjwdxnbt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#corjwdxnbt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#corjwdxnbt .gt_left {
  text-align: left;
}

#corjwdxnbt .gt_center {
  text-align: center;
}

#corjwdxnbt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#corjwdxnbt .gt_font_normal {
  font-weight: normal;
}

#corjwdxnbt .gt_font_bold {
  font-weight: bold;
}

#corjwdxnbt .gt_font_italic {
  font-style: italic;
}

#corjwdxnbt .gt_super {
  font-size: 65%;
}

#corjwdxnbt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#corjwdxnbt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#corjwdxnbt .gt_indent_1 {
  text-indent: 5px;
}

#corjwdxnbt .gt_indent_2 {
  text-indent: 10px;
}

#corjwdxnbt .gt_indent_3 {
  text-indent: 15px;
}

#corjwdxnbt .gt_indent_4 {
  text-indent: 20px;
}

#corjwdxnbt .gt_indent_5 {
  text-indent: 25px;
}

#corjwdxnbt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#corjwdxnbt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n"><span class='gt_from_md'><strong>N</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'><strong>EPOCH COS up to 5 years</strong><br />
N = 263</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'><strong>Infant feeding COS</strong><br />
N = 247</span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Facilitator</td>
<td headers="n" class="gt_row gt_center">510</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    None</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">3/263 (1.1%)</td>
<td headers="stat_2" class="gt_row gt_center">1/247 (0.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Being previously involved in the development of a different COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">4/263 (1.5%)</td>
<td headers="stat_2" class="gt_row gt_center">3/247 (1.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other, please specify</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">3/263 (1.1%)</td>
<td headers="stat_2" class="gt_row gt_center">4/247 (1.6%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Previously using a different COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">5/263 (1.9%)</td>
<td headers="stat_2" class="gt_row gt_center">2/247 (0.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Knowing why to use the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">6/263 (2.3%)</td>
<td headers="stat_2" class="gt_row gt_center">6/247 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Being previously involved in the development of the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">9/263 (3.4%)</td>
<td headers="stat_2" class="gt_row gt_center">8/247 (3.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Having support for use of the COS in the research team</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">13/263 (4.9%)</td>
<td headers="stat_2" class="gt_row gt_center">11/247 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Understanding what the COS is</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">13/263 (4.9%)</td>
<td headers="stat_2" class="gt_row gt_center">11/247 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Association of the COS with recognised groups</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">14/263 (5.3%)</td>
<td headers="stat_2" class="gt_row gt_center">11/247 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Knowing other researchers who have used the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">14/263 (5.3%)</td>
<td headers="stat_2" class="gt_row gt_center">14/247 (5.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS is relevant for your geographical location(s)/resource setting(s)</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">14/263 (5.3%)</td>
<td headers="stat_2" class="gt_row gt_center">15/247 (6.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Recommendation from professional body to use the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">17/263 (6.5%)</td>
<td headers="stat_2" class="gt_row gt_center">16/247 (6.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Understanding how to use the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">18/263 (6.8%)</td>
<td headers="stat_2" class="gt_row gt_center">18/247 (7.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Recommendation from funding body to use the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">19/263 (7.2%)</td>
<td headers="stat_2" class="gt_row gt_center">18/247 (7.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS is relevant for the type(s) of intervention you are examining</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">20/263 (7.6%)</td>
<td headers="stat_2" class="gt_row gt_center">20/247 (8.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Seeing the COS previously used in other trials</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">20/263 (7.6%)</td>
<td headers="stat_2" class="gt_row gt_center">20/247 (8.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling that the outcomes in the COS are the most important outcomes to measure</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">23/263 (8.7%)</td>
<td headers="stat_2" class="gt_row gt_center">20/247 (8.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Feeling the COS is relevant for your population(s) of interest</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">21/263 (8.0%)</td>
<td headers="stat_2" class="gt_row gt_center">22/247 (8.9%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Having guidelines and resources available to support using the COS</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center">27/263 (10%)</td>
<td headers="stat_2" class="gt_row gt_center">27/247 (11%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>510 facilitators endorsed by 45 respondents</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

#### Free text


```
## [1] "more funding; clear-cut scientific evidence that they are effective in long-term studies, i.e., 4 years or more. There is no association between short term and long term studies. In many reviews 6 month outcomes are used to demonstrate a preventive effect. 6 months is too short to demonstrate a reliable weight-loss treatment effect and of course not relevant for prevention.; Short form tools to measure outcomes. Item bank for flexibility to contextually select measures; Availability of validated instruments to measure the outcomes in my langage; Knowing the specific questionnaires to use"
```

## (Q17) Sources to identify outcomes in childhood obesity prevention trials 

#### Figure: Sources to identify outcomes in childhood obesity prevention trials

![](trialists_survey_files/figure-html/unnamed-chunk-41-1.png)<!-- -->


```{=html}
<div id="uvwwuxuqil" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uvwwuxuqil table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#uvwwuxuqil thead, #uvwwuxuqil tbody, #uvwwuxuqil tfoot, #uvwwuxuqil tr, #uvwwuxuqil td, #uvwwuxuqil th {
  border-style: none;
}

#uvwwuxuqil p {
  margin: 0;
  padding: 0;
}

#uvwwuxuqil .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uvwwuxuqil .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#uvwwuxuqil .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uvwwuxuqil .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uvwwuxuqil .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uvwwuxuqil .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uvwwuxuqil .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uvwwuxuqil .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uvwwuxuqil .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uvwwuxuqil .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uvwwuxuqil .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uvwwuxuqil .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uvwwuxuqil .gt_spanner_row {
  border-bottom-style: hidden;
}

#uvwwuxuqil .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#uvwwuxuqil .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uvwwuxuqil .gt_from_md > :first-child {
  margin-top: 0;
}

#uvwwuxuqil .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uvwwuxuqil .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uvwwuxuqil .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#uvwwuxuqil .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#uvwwuxuqil .gt_row_group_first td {
  border-top-width: 2px;
}

#uvwwuxuqil .gt_row_group_first th {
  border-top-width: 2px;
}

#uvwwuxuqil .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uvwwuxuqil .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uvwwuxuqil .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uvwwuxuqil .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uvwwuxuqil .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uvwwuxuqil .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uvwwuxuqil .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#uvwwuxuqil .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uvwwuxuqil .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uvwwuxuqil .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uvwwuxuqil .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uvwwuxuqil .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uvwwuxuqil .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uvwwuxuqil .gt_left {
  text-align: left;
}

#uvwwuxuqil .gt_center {
  text-align: center;
}

#uvwwuxuqil .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uvwwuxuqil .gt_font_normal {
  font-weight: normal;
}

#uvwwuxuqil .gt_font_bold {
  font-weight: bold;
}

#uvwwuxuqil .gt_font_italic {
  font-style: italic;
}

#uvwwuxuqil .gt_super {
  font-size: 65%;
}

#uvwwuxuqil .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#uvwwuxuqil .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uvwwuxuqil .gt_indent_1 {
  text-indent: 5px;
}

#uvwwuxuqil .gt_indent_2 {
  text-indent: 10px;
}

#uvwwuxuqil .gt_indent_3 {
  text-indent: 15px;
}

#uvwwuxuqil .gt_indent_4 {
  text-indent: 20px;
}

#uvwwuxuqil .gt_indent_5 {
  text-indent: 25px;
}

#uvwwuxuqil .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#uvwwuxuqil div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="n"><span class='gt_from_md'><strong>N</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 235</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Sources</td>
<td headers="n" class="gt_row gt_center">235</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outcomes measurement instruments used in other trials</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">41/235 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Information from a feasibility/pilot study</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">38/235 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outcomes measurement instruments used in systematic reviews</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">33/235 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outcomes measurement instruments used in other study designs</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">29/235 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Outcome measurement instruments developed by our team members</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">25/235 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Patient and public perspective</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">20/235 (8.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Practitioner perspective</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">17/235 (7.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Recommendation(s) from a professional body</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">12/235 (5.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Recommendations from regulatory body</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">10/235 (4.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Recommendation(s) from a funding body</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">8/235 (3.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other, please specify</td>
<td headers="n" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center">2/235 (0.9%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>235 sources endorsed by 46 respondents</span></td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>2</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```


Note: No actual free text field for "other" responses. 

## (Q18) Currently planned/used outcomes 

#### Figure: Currently planned/used outcomes in childhood obesity prevention interventions.

![](trialists_survey_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

#### Free text


```
## [1] "Physical tests: exercise tests, motor function tests"
```

## (Q19) Administration 

#### Figure: Route and method of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

#### Table: Route and method of administration across all responses


```{=html}
<div id="skteevkbnx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#skteevkbnx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#skteevkbnx thead, #skteevkbnx tbody, #skteevkbnx tfoot, #skteevkbnx tr, #skteevkbnx td, #skteevkbnx th {
  border-style: none;
}

#skteevkbnx p {
  margin: 0;
  padding: 0;
}

#skteevkbnx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#skteevkbnx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#skteevkbnx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#skteevkbnx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#skteevkbnx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#skteevkbnx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#skteevkbnx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#skteevkbnx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#skteevkbnx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#skteevkbnx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#skteevkbnx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#skteevkbnx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#skteevkbnx .gt_spanner_row {
  border-bottom-style: hidden;
}

#skteevkbnx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#skteevkbnx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#skteevkbnx .gt_from_md > :first-child {
  margin-top: 0;
}

#skteevkbnx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#skteevkbnx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#skteevkbnx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#skteevkbnx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#skteevkbnx .gt_row_group_first td {
  border-top-width: 2px;
}

#skteevkbnx .gt_row_group_first th {
  border-top-width: 2px;
}

#skteevkbnx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#skteevkbnx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#skteevkbnx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#skteevkbnx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#skteevkbnx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#skteevkbnx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#skteevkbnx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#skteevkbnx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#skteevkbnx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#skteevkbnx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#skteevkbnx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#skteevkbnx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#skteevkbnx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#skteevkbnx .gt_left {
  text-align: left;
}

#skteevkbnx .gt_center {
  text-align: center;
}

#skteevkbnx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#skteevkbnx .gt_font_normal {
  font-weight: normal;
}

#skteevkbnx .gt_font_bold {
  font-weight: bold;
}

#skteevkbnx .gt_font_italic {
  font-style: italic;
}

#skteevkbnx .gt_super {
  font-size: 65%;
}

#skteevkbnx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#skteevkbnx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#skteevkbnx .gt_indent_1 {
  text-indent: 5px;
}

#skteevkbnx .gt_indent_2 {
  text-indent: 10px;
}

#skteevkbnx .gt_indent_3 {
  text-indent: 15px;
}

#skteevkbnx .gt_indent_4 {
  text-indent: 20px;
}

#skteevkbnx .gt_indent_5 {
  text-indent: 25px;
}

#skteevkbnx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#skteevkbnx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5" scope="colgroup" id="Method">
        <div class="gt_column_spanner"><span class='gt_from_md'>Method</span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'>Total</span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'>Anthropometric measures</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'>Diary approaches</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'>Direct observation</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_4"><span class='gt_from_md'>Questionnaires/ surveys/ scales</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_5"><span class='gt_from_md'>Structured recall approaches</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Route</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    By phone</td>
<td headers="stat_1" class="gt_row gt_center">6 (2.1%)</td>
<td headers="stat_2" class="gt_row gt_center">2 (0.7%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0%)</td>
<td headers="stat_4" class="gt_row gt_center">17 (5.9%)</td>
<td headers="stat_5" class="gt_row gt_center">18 (6.3%)</td>
<td headers="stat_0" class="gt_row gt_center">43 (15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Combination</td>
<td headers="stat_1" class="gt_row gt_center">9 (3.1%)</td>
<td headers="stat_2" class="gt_row gt_center">5 (1.7%)</td>
<td headers="stat_3" class="gt_row gt_center">3 (1.0%)</td>
<td headers="stat_4" class="gt_row gt_center">10 (3.5%)</td>
<td headers="stat_5" class="gt_row gt_center">6 (2.1%)</td>
<td headers="stat_0" class="gt_row gt_center">33 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Electronically</td>
<td headers="stat_1" class="gt_row gt_center">10 (3.5%)</td>
<td headers="stat_2" class="gt_row gt_center">14 (4.9%)</td>
<td headers="stat_3" class="gt_row gt_center">4 (1.4%)</td>
<td headers="stat_4" class="gt_row gt_center">40 (14%)</td>
<td headers="stat_5" class="gt_row gt_center">19 (6.6%)</td>
<td headers="stat_0" class="gt_row gt_center">87 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    In person</td>
<td headers="stat_1" class="gt_row gt_center">37 (13%)</td>
<td headers="stat_2" class="gt_row gt_center">9 (3.1%)</td>
<td headers="stat_3" class="gt_row gt_center">20 (6.9%)</td>
<td headers="stat_4" class="gt_row gt_center">26 (9.0%)</td>
<td headers="stat_5" class="gt_row gt_center">14 (4.9%)</td>
<td headers="stat_0" class="gt_row gt_center">106 (37%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Via post</td>
<td headers="stat_1" class="gt_row gt_center">2 (0.7%)</td>
<td headers="stat_2" class="gt_row gt_center">6 (2.1%)</td>
<td headers="stat_3" class="gt_row gt_center">1 (0.3%)</td>
<td headers="stat_4" class="gt_row gt_center">7 (2.4%)</td>
<td headers="stat_5" class="gt_row gt_center">3 (1.0%)</td>
<td headers="stat_0" class="gt_row gt_center">19 (6.6%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total</td>
<td headers="stat_1" class="gt_row gt_center">64 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">36 (13%)</td>
<td headers="stat_3" class="gt_row gt_center">28 (9.7%)</td>
<td headers="stat_4" class="gt_row gt_center">100 (35%)</td>
<td headers="stat_5" class="gt_row gt_center">60 (21%)</td>
<td headers="stat_0" class="gt_row gt_center">288 (100%)</td></tr>
  </tbody>
  
  
</table>
</div>
```

#### Figure: Route and method of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

#### Figure: Method of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

#### Figure: Route of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

#### Figure: Method of administration - In person

![](trialists_survey_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

#### Figure: Method of administration - Electronically

![](trialists_survey_files/figure-html/unnamed-chunk-51-1.png)<!-- -->

#### Figure: Method of administration - By phone

![](trialists_survey_files/figure-html/unnamed-chunk-52-1.png)<!-- -->

#### Figure: Method of administration - Via post

![](trialists_survey_files/figure-html/unnamed-chunk-53-1.png)<!-- -->

#### Figure: Method of administration - Combination

![](trialists_survey_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

#### Free text


```
## [1] NA                                                                   
## [2] "0"                                                                  
## [3] "Anthropometric measures"                                            
## [4] "Gave participants options of how they wanted to complete the survey"
## [5] "from usual pediatric visit"
```

## (Q20) Location 

#### Figure: Method and setting of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-56-1.png)<!-- -->

#### Table: Route and method of administration across all responses


```{=html}
<div id="hmvhqatwrh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hmvhqatwrh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hmvhqatwrh thead, #hmvhqatwrh tbody, #hmvhqatwrh tfoot, #hmvhqatwrh tr, #hmvhqatwrh td, #hmvhqatwrh th {
  border-style: none;
}

#hmvhqatwrh p {
  margin: 0;
  padding: 0;
}

#hmvhqatwrh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hmvhqatwrh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hmvhqatwrh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hmvhqatwrh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hmvhqatwrh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hmvhqatwrh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmvhqatwrh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hmvhqatwrh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hmvhqatwrh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hmvhqatwrh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hmvhqatwrh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hmvhqatwrh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hmvhqatwrh .gt_spanner_row {
  border-bottom-style: hidden;
}

#hmvhqatwrh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hmvhqatwrh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hmvhqatwrh .gt_from_md > :first-child {
  margin-top: 0;
}

#hmvhqatwrh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hmvhqatwrh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hmvhqatwrh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hmvhqatwrh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hmvhqatwrh .gt_row_group_first td {
  border-top-width: 2px;
}

#hmvhqatwrh .gt_row_group_first th {
  border-top-width: 2px;
}

#hmvhqatwrh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmvhqatwrh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hmvhqatwrh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hmvhqatwrh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmvhqatwrh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmvhqatwrh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hmvhqatwrh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hmvhqatwrh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hmvhqatwrh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hmvhqatwrh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hmvhqatwrh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmvhqatwrh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hmvhqatwrh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hmvhqatwrh .gt_left {
  text-align: left;
}

#hmvhqatwrh .gt_center {
  text-align: center;
}

#hmvhqatwrh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hmvhqatwrh .gt_font_normal {
  font-weight: normal;
}

#hmvhqatwrh .gt_font_bold {
  font-weight: bold;
}

#hmvhqatwrh .gt_font_italic {
  font-style: italic;
}

#hmvhqatwrh .gt_super {
  font-size: 65%;
}

#hmvhqatwrh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hmvhqatwrh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hmvhqatwrh .gt_indent_1 {
  text-indent: 5px;
}

#hmvhqatwrh .gt_indent_2 {
  text-indent: 10px;
}

#hmvhqatwrh .gt_indent_3 {
  text-indent: 15px;
}

#hmvhqatwrh .gt_indent_4 {
  text-indent: 20px;
}

#hmvhqatwrh .gt_indent_5 {
  text-indent: 25px;
}

#hmvhqatwrh .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hmvhqatwrh div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="label"></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5" scope="colgroup" id="Method">
        <div class="gt_column_spanner"><span class='gt_from_md'>Method</span></div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'>Total</span></th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_1"><span class='gt_from_md'>Anthropometric measures</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_2"><span class='gt_from_md'>Diary approaches</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_3"><span class='gt_from_md'>Direct observation</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_4"><span class='gt_from_md'>Questionnaires/ surveys /scales</span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_5"><span class='gt_from_md'>Structured recall approaches</span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Setting</td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Academic/research</td>
<td headers="stat_1" class="gt_row gt_center">15 (4.3%)</td>
<td headers="stat_2" class="gt_row gt_center">8 (2.3%)</td>
<td headers="stat_3" class="gt_row gt_center">8 (2.3%)</td>
<td headers="stat_4" class="gt_row gt_center">18 (5.2%)</td>
<td headers="stat_5" class="gt_row gt_center">11 (3.2%)</td>
<td headers="stat_0" class="gt_row gt_center">60 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Combination</td>
<td headers="stat_1" class="gt_row gt_center">6 (1.7%)</td>
<td headers="stat_2" class="gt_row gt_center">3 (0.9%)</td>
<td headers="stat_3" class="gt_row gt_center">2 (0.6%)</td>
<td headers="stat_4" class="gt_row gt_center">7 (2.0%)</td>
<td headers="stat_5" class="gt_row gt_center">5 (1.4%)</td>
<td headers="stat_0" class="gt_row gt_center">23 (6.6%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Community</td>
<td headers="stat_1" class="gt_row gt_center">14 (4.0%)</td>
<td headers="stat_2" class="gt_row gt_center">5 (1.4%)</td>
<td headers="stat_3" class="gt_row gt_center">7 (2.0%)</td>
<td headers="stat_4" class="gt_row gt_center">17 (4.9%)</td>
<td headers="stat_5" class="gt_row gt_center">9 (2.6%)</td>
<td headers="stat_0" class="gt_row gt_center">52 (15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Education</td>
<td headers="stat_1" class="gt_row gt_center">8 (2.3%)</td>
<td headers="stat_2" class="gt_row gt_center">2 (0.6%)</td>
<td headers="stat_3" class="gt_row gt_center">8 (2.3%)</td>
<td headers="stat_4" class="gt_row gt_center">12 (3.5%)</td>
<td headers="stat_5" class="gt_row gt_center">4 (1.2%)</td>
<td headers="stat_0" class="gt_row gt_center">34 (9.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Healthcare</td>
<td headers="stat_1" class="gt_row gt_center">28 (8.1%)</td>
<td headers="stat_2" class="gt_row gt_center">10 (2.9%)</td>
<td headers="stat_3" class="gt_row gt_center">11 (3.2%)</td>
<td headers="stat_4" class="gt_row gt_center">29 (8.4%)</td>
<td headers="stat_5" class="gt_row gt_center">17 (4.9%)</td>
<td headers="stat_0" class="gt_row gt_center">95 (27%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Home</td>
<td headers="stat_1" class="gt_row gt_center">17 (4.9%)</td>
<td headers="stat_2" class="gt_row gt_center">14 (4.0%)</td>
<td headers="stat_3" class="gt_row gt_center">11 (3.2%)</td>
<td headers="stat_4" class="gt_row gt_center">26 (7.5%)</td>
<td headers="stat_5" class="gt_row gt_center">15 (4.3%)</td>
<td headers="stat_0" class="gt_row gt_center">83 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total</td>
<td headers="stat_1" class="gt_row gt_center">88 (25%)</td>
<td headers="stat_2" class="gt_row gt_center">42 (12%)</td>
<td headers="stat_3" class="gt_row gt_center">47 (14%)</td>
<td headers="stat_4" class="gt_row gt_center">109 (31%)</td>
<td headers="stat_5" class="gt_row gt_center">61 (18%)</td>
<td headers="stat_0" class="gt_row gt_center">347 (100%)</td></tr>
  </tbody>
  
  
</table>
</div>
```

#### Figure: Route and method of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-58-1.png)<!-- -->

#### Figure: Method of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-59-1.png)<!-- -->

#### Figure: Location of administration across all responses

![](trialists_survey_files/figure-html/unnamed-chunk-60-1.png)<!-- -->

#### Figure: Method of administration - Healthcare

![](trialists_survey_files/figure-html/unnamed-chunk-61-1.png)<!-- -->

#### Figure: Method of administration - Home

![](trialists_survey_files/figure-html/unnamed-chunk-62-1.png)<!-- -->

#### Figure: Method of administration - Community

![](trialists_survey_files/figure-html/unnamed-chunk-63-1.png)<!-- -->

#### Figure: Method of administration - Educational

![](trialists_survey_files/figure-html/unnamed-chunk-64-1.png)<!-- -->

#### Figure: Method of administration - Academic/research

![](trialists_survey_files/figure-html/unnamed-chunk-65-1.png)<!-- -->

#### Figure: Method of administration - Combination

![](trialists_survey_files/figure-html/unnamed-chunk-66-1.png)<!-- -->

#### Free text


```
## [1] "Questionnaires/ surveys /scales" "Structured recall approaches"   
## [3] "Diary approaches"                "0"                              
## [5] "Anthropometric measures"         "Online so no location per say"
```

## (Q21) Importance 


```{=html}
<div id="lruwqxbevb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lruwqxbevb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lruwqxbevb thead, #lruwqxbevb tbody, #lruwqxbevb tfoot, #lruwqxbevb tr, #lruwqxbevb td, #lruwqxbevb th {
  border-style: none;
}

#lruwqxbevb p {
  margin: 0;
  padding: 0;
}

#lruwqxbevb .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lruwqxbevb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lruwqxbevb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lruwqxbevb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lruwqxbevb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lruwqxbevb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lruwqxbevb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lruwqxbevb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lruwqxbevb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lruwqxbevb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lruwqxbevb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lruwqxbevb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lruwqxbevb .gt_spanner_row {
  border-bottom-style: hidden;
}

#lruwqxbevb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lruwqxbevb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lruwqxbevb .gt_from_md > :first-child {
  margin-top: 0;
}

#lruwqxbevb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lruwqxbevb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lruwqxbevb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lruwqxbevb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lruwqxbevb .gt_row_group_first td {
  border-top-width: 2px;
}

#lruwqxbevb .gt_row_group_first th {
  border-top-width: 2px;
}

#lruwqxbevb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lruwqxbevb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lruwqxbevb .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lruwqxbevb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lruwqxbevb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lruwqxbevb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lruwqxbevb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lruwqxbevb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lruwqxbevb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lruwqxbevb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lruwqxbevb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lruwqxbevb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lruwqxbevb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lruwqxbevb .gt_left {
  text-align: left;
}

#lruwqxbevb .gt_center {
  text-align: center;
}

#lruwqxbevb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lruwqxbevb .gt_font_normal {
  font-weight: normal;
}

#lruwqxbevb .gt_font_bold {
  font-weight: bold;
}

#lruwqxbevb .gt_font_italic {
  font-style: italic;
}

#lruwqxbevb .gt_super {
  font-size: 65%;
}

#lruwqxbevb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lruwqxbevb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lruwqxbevb .gt_indent_1 {
  text-indent: 5px;
}

#lruwqxbevb .gt_indent_2 {
  text-indent: 10px;
}

#lruwqxbevb .gt_indent_3 {
  text-indent: 15px;
}

#lruwqxbevb .gt_indent_4 {
  text-indent: 20px;
}

#lruwqxbevb .gt_indent_5 {
  text-indent: 25px;
}

#lruwqxbevb .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#lruwqxbevb div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 46</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Ease of finding/identifying the outcome measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">19/44 (43%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">18/44 (41%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">7/44 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Whether the measurement instrument has been used in similar trials previously</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">25/45 (56%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">18/45 (40%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">2/45 (4.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/45 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/45 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">How the measurement instrument was developed</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">20/44 (45%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">19/44 (43%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">2/44 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">3/44 (6.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Who developed the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">4/43 (9.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">6/43 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">20/43 (47%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">12/43 (28%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">1/43 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Relevance of the measurement instrument for your population</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">34/45 (76%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">10/45 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">1/45 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/45 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/45 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Whether the measurement instrument requires translation for your trial</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">11/43 (26%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">19/43 (44%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">11/43 (26%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">2/43 (4.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Whether there is support for multiple languages when using the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">6/43 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">18/43 (42%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">13/43 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">5/43 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">1/43 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Cultural sensitivity and appropriateness</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">22/44 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">19/44 (43%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">3/44 (6.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ease of administration</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">34/44 (77%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">8/44 (18%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">2/44 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The number of items in the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">29/45 (64%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">14/45 (31%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">2/45 (4.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/45 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/45 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">1</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Length of time for researcher to administer</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">20/43 (47%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">18/43 (42%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">5/43 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Length of time for participants to complete</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">34/44 (77%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">10/44 (23%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Participants comprehensibility of the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">28/44 (64%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">14/44 (32%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">2/44 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Clinicians comprehensibility of the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">10/42 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">20/42 (48%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">7/42 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">4/42 (9.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">1/42 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Participants mental ability to complete the measurement</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">24/43 (56%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">16/43 (37%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">3/43 (7.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Participants physical ability to complete the measurement</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">19/42 (45%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">16/42 (38%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">6/42 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">1/42 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Whether the measurement instrument requires specialist training to administer</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">13/42 (31%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">23/42 (55%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">5/42 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">1/42 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Whether the measurement instrument requires specialist equipment/device(s) to administer</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">14/40 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">21/40 (53%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">3/40 (7.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">2/40 (5.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/40 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">6</td></tr>
    <tr><td headers="label" class="gt_row gt_left">If a designated place is needed to administer/complete the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">17/42 (40%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">16/42 (38%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">7/42 (17%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">2/42 (4.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The mode of administration (i.e., in person, online, by phone)</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">14/43 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">20/43 (47%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">9/43 (21%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">When the measurement instrument is completed</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">9/43 (21%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">18/43 (42%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">13/43 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">3/43 (7.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Acceptability of the measurement instrument to clinicians/practice staff</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">16/44 (36%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">19/44 (43%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">8/44 (18%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">1/44 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Acceptability of the measurement instrument to participants</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">29/43 (67%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">11/43 (26%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">3/43 (7.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Costs associated with using the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">19/43 (44%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">24/43 (56%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">How the instrument handles sensitive information</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">24/44 (55%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">13/44 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">4/44 (9.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">3/44 (6.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Risk of adverse outcomes following use of the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">21/42 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">16/42 (38%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">4/42 (9.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">1/42 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Ease of score calculation</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">3/44 (6.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">22/44 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">12/44 (27%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">5/44 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">2/44 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The types of analyses that can be conducted with the data obtained</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">19/44 (43%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">20/44 (45%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">5/44 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Likelihood of significant findings from using the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">17/41 (41%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">12/41 (29%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">10/41 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">1/41 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">1/41 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">5</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The response format of the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">2/44 (4.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">21/44 (48%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">12/44 (27%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">8/44 (18%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">1/44 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The recall period for the measurement instrument</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">5/44 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">22/44 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">12/44 (27%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">5/44 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/44 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">2</td></tr>
    <tr><td headers="label" class="gt_row gt_left">How well the measurement instrument reflects the construct being measured</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">34/43 (79%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">8/43 (19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">1/43 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The degree to which the measurement instrument is free from measurement error</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">29/43 (67%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">14/43 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The ability of the measurement instrument to detect change over time</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">34/43 (79%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">7/43 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">2/43 (4.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The degree that the measurement instrument reflects a gold standard</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">28/42 (67%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">9/42 (21%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">5/42 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The degree that a culturally adapted measurement instrument reflects the original version</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">14/42 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">25/42 (60%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">3/42 (7.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/42 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">4</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The degree of interrelatedness among the items in the measure</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">14/43 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">22/43 (51%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">6/43 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">1/43 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
    <tr><td headers="label" class="gt_row gt_left">The degree that the measurement instrument reflects the dimensionality of the construct</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Very important</td>
<td headers="stat_0" class="gt_row gt_center">13/43 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Important</td>
<td headers="stat_0" class="gt_row gt_center">24/43 (56%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Neutral</td>
<td headers="stat_0" class="gt_row gt_center">6/43 (14%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Not important at all</td>
<td headers="stat_0" class="gt_row gt_center">0/43 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Missing or not applicable</td>
<td headers="stat_0" class="gt_row gt_center">3</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```



![](trialists_survey_files/figure-html/unnamed-chunk-70-1.png)<!-- -->

## (Q22) Other influences on use of core outcome sets 

#### Free text


```
##  [1] "nothing to add"                                                                                                                                                                                                         
##  [2] "Need for better measurement of breast/chestfeeding and use/timing of commercial milk formula; move beyond breastfeeding duration and EBF through 3 or 6 months (yes/no)"                                                
##  [3] "The accessibility of the measures and appropriateness for use in children with learning difficulties or disability is important and guides whether I would use depending on the population in question."                
##  [4] "No"                                                                                                                                                                                                                     
##  [5] "I have never used the term \\x93core outcome set\\x94 but have used them for years so tried to reflect that in my responses."                                                                                           
##  [6] "What is most appropriate for my context, study, hypothesis, etc"                                                                                                                                                        
##  [7] "I need to remember it by the time I undertake intervention planning - so the COS needs to be searchable (and top of mind! - good SEO) when I review the current literature in the field to develop outcome assessments."
##  [8] "Participant burden should be considered, and trial setting (e.g., hospital, primary health care, online) affects participants attention."                                                                               
##  [9] "Main factors are\\nOutcomes should reflect intervention targets & inform efficacy\\nShould be feasible to use (valid, acceptable & affordable)\\n"                                                                      
## [10] "Nothing to report"                                                                                                                                                                                                      
## [11] "You are lacking health related quality of life."                                                                                                                                                                        
## [12] "The availability (or lack thereof) of cultural adaptation of validated instruments (e.g., questionnaire) is a key barrier for use of core outcomes in my trial."                                                        
## [13] "Thank you. No The survey is very comprehensive"
```

## (Q23) Other influences on choice of outcomes 

#### Free text


```
## [1] "No\\n"                                                                                                                                                                                                                                                                                                                                                                                                          
## [2] "The importance of objective measurement of health parameters/domains beyond just anthropometry....eg blood pressure, respiratory function, functional capacity."                                                                                                                                                                                                                                                
## [3] "No"                                                                                                                                                                                                                                                                                                                                                                                                             
## [4] "Participant feedback from previous trials on uncertainty about how to answer specific questions, questions are not applicable, or caregiver states that they respond variably in different situations not captured by questionnaire, etc. Additionally, lack of findings from existing measures considered core outcomes throughout the literature. Would love to learn more about the results from this study!"
## [5] "Depends on the specific aims of the project. Within obesity prevention there can be many many areas of focus, so my choice of outcome measures depends more on what the overall goal is than on any the criteria you asked about."                                                                                                                                                                              
## [6] "Funding and human resources available for data collection and analysis.\\n\\n"                                                                                                                                                                                                                                                                                                                                  
## [7] "Age-appropriateness to specific stages of childhood obesity, e.g. age where children are expected to have transitioned from bottles to cups."                                                                                                                                                                                                                                                                   
## [8] "Nothing to report"                                                                                                                                                                                                                                                                                                                                                                                              
## [9] "You have to better describe different anthropometric methods."
```

## Where respondents learned about the survey (Q24)


#### Table: Where respondents learned about the survey.


```{=html}
<div id="xhirnyztal" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xhirnyztal table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xhirnyztal thead, #xhirnyztal tbody, #xhirnyztal tfoot, #xhirnyztal tr, #xhirnyztal td, #xhirnyztal th {
  border-style: none;
}

#xhirnyztal p {
  margin: 0;
  padding: 0;
}

#xhirnyztal .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xhirnyztal .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xhirnyztal .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xhirnyztal .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xhirnyztal .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xhirnyztal .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xhirnyztal .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xhirnyztal .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xhirnyztal .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xhirnyztal .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xhirnyztal .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xhirnyztal .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xhirnyztal .gt_spanner_row {
  border-bottom-style: hidden;
}

#xhirnyztal .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#xhirnyztal .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xhirnyztal .gt_from_md > :first-child {
  margin-top: 0;
}

#xhirnyztal .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xhirnyztal .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xhirnyztal .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#xhirnyztal .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#xhirnyztal .gt_row_group_first td {
  border-top-width: 2px;
}

#xhirnyztal .gt_row_group_first th {
  border-top-width: 2px;
}

#xhirnyztal .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhirnyztal .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xhirnyztal .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xhirnyztal .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xhirnyztal .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhirnyztal .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xhirnyztal .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xhirnyztal .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xhirnyztal .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xhirnyztal .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xhirnyztal .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhirnyztal .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xhirnyztal .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xhirnyztal .gt_left {
  text-align: left;
}

#xhirnyztal .gt_center {
  text-align: center;
}

#xhirnyztal .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xhirnyztal .gt_font_normal {
  font-weight: normal;
}

#xhirnyztal .gt_font_bold {
  font-weight: bold;
}

#xhirnyztal .gt_font_italic {
  font-style: italic;
}

#xhirnyztal .gt_super {
  font-size: 65%;
}

#xhirnyztal .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xhirnyztal .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xhirnyztal .gt_indent_1 {
  text-indent: 5px;
}

#xhirnyztal .gt_indent_2 {
  text-indent: 10px;
}

#xhirnyztal .gt_indent_3 {
  text-indent: 15px;
}

#xhirnyztal .gt_indent_4 {
  text-indent: 20px;
}

#xhirnyztal .gt_indent_5 {
  text-indent: 25px;
}

#xhirnyztal .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#xhirnyztal div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="stat_0"><span class='gt_from_md'><strong>N = 45</strong></span><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">How did you hear about this survey?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Received information from organisation/society</td>
<td headers="stat_0" class="gt_row gt_center">1/45 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Direct email invitation</td>
<td headers="stat_0" class="gt_row gt_center">44/45 (98%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height:0;"><sup>1</sup></span> <span class='gt_from_md'>n/N Non-missing (%)</span></td>
    </tr>
  </tfoot>
</table>
</div>
```

