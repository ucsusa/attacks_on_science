Readme for Attacks on Science Tracker Data Process
================
06/22/2026

##### Last updated: 06/22/2026

The Union of Concerned Scientists (UCS) has been documenting federal
attacks on science and advocating for scientific integrity protections
since 2001. In response to the second Trump administration’s systemic
attack on the federal scientific enterprise, the Center for Science and
Democracy at UCS evolved its methodology to better capture the greater
number and types of attacks on science committed by the second Trump
administration.

The scripts in this repository collect RSS feeds from select news
outlets, screen articles for search terms to identify which contain
potential attacks on science, organize screened articles into a
spreadsheet to be analyzed by trained human coders, and generate various
meta and descriptive data. Fully coded and reviewed data are formatted
to include in the [Attacks on Science
Tracker](https://www.attacksonscience.org/).

- The UCS Attacks on Science Tracker, showing attacks and potential
  scientific integrity violations committed by the second Trump
  administration, can be found
  [here](https://www.attacksonscience.org/).

- A full description of the Tracker’s methodology that apply these R
  scripts can be found
  [here](http://www.ucs.org/resources/attacks-science-methodology).

- The data that support the Attacks on Science Tracker can be found in
  [Harvard Dataverse](https://doi.org/10.7910/DVN/PFIZPS). It will be
  routinely updated.

- UCS’s past work tracking Attacks on Science can be found
  [here](https://www.ucs.org/resources/attacks-on-science).

The below table summarizes each R script in the repository by name, a
short description, and how frequently it runs.

| Name | Description | Frequency |
|:---|:---|:---|
| 01_read_in_rssfeed | Grabs RSS feeds from our targeted news sources daily and puts into a table | 1x/day |
| 02_scan_descriptions_keywords | Pulls table of daily RSS feeds (from Script 01) and screens RSS descriptions/titles using first AOS search term criteria | 1x/day |
| 04_read_screened_urls | Compiles article RSS feeds that passed first AOS search term filter (in script 02) and collects article text via targeted URL scraping | 1x/week |
| 05_read_in_saved_pdfs | Saves pdfs of full article text of unread or misread articles from previous script | 1x/week |
| 06_screen_in_read_urls_full_search_terms | Screens full article text (from scraping and pdf saving) using second AOS search term criteria | 1x/week |
| 07_clean_up_raw_aos_text | Clean up article text that contain potential attacks on science to prepare for subsequent analysis | 1x/week |
| 08_tag_govt_agency | Documents federal agencies mentioned in first 1/3 of article text | 1x/week |
| 09_tag_SI_GSS | Tags articles (TRUE/FALSE) with mentions of gold standard science or scientific integrity in article text | 1x/week |
| 10_aggregate_aos_date_coding | Pulls the most recent 2 weeks of RSS feed articles and makes groups of articles based on: articles +/- 2 days of publication (to approximate a news cycle) and with 30% matching words in the title and/or descriptions to identify potential duplicate attacks | 1x/week |
| 11_human_coding_spreadsheet | Formats screened articles containing a potential attack on science into a spreadsheet for human coders to review | 1x/week |
| 12_flag_aos_multiples | Runs a second duplicate check: flags articles coded in the same way and occur within +/- 2 days | 1x/week |
| 13_assign_unique_id | Assigns unique identifiers to each human-coded attack on science | 1x/week |
| 14_save_aos_citations | Saves meta data of each article that contain an attack on science, confirmed by human coders | 1x/week |
| 15_tag_si_violations | Identifies potential scientific integrity violations based on attacks on science types identified by human coders | 1x/week |
| 16_add_title_summary_lists | Creates attack summaries by pasting abbreviated definitions of attack on science variables into sentence structure | 1x/week |
| 17_modify_date | Modifies the date of reporting to a week, month, and a year | 1x/week |
| 18_combine_human_rss_coding_data_sets | Combines and formats the data from human-driven and automated data collection | 1x/week |
| 19_make_a_workbook | Creates a workbook with a readme from the combined data from human-driven and automated data collection for Attacks on Science Tracker | 1x/week |
| ID_negative_verbs | Identified most common verbs used in articles (and their headlines) from Jan-Apr 2025 that humans confirmed contained an attack on science. This was used to inform the initial list of negative verb search terms | N/A |
