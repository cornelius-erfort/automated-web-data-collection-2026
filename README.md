Automated Web Data Collection

# Course Description

The internet is an essential source of data for social science research, providing access to vast amounts of text and structured information. This course introduces students to methods for automated web data collection, focusing on practical applications in political science and other social sciences. Students will learn web scraping techniques, work with APIs, and process various data formats. Ethical and legal considerations will also be discussed.

## Workshop Files (Latest Versions)

- [Day 1 Slides (HTML, rendered)](https://htmlpreview.github.io/?https://github.com/cornelius-erfort/automated-web-data-collection/blob/main/slides/day-one.html)
- [Day 2 Slides (HTML, rendered)](https://htmlpreview.github.io/?https://github.com/cornelius-erfort/automated-web-data-collection/blob/main/slides/day-two.html)
- [Syllabus (PDF)](Workshop_Syllabus__Automated_Web_Data_Collection.pdf)

# Course Outline Day 1

- HTML and Web Structure: Basics of HTML and CSS, `rvest`
  - HTML basics ([r4ds.hadley.nz/webscraping.html#html-basics](https://r4ds.hadley.nz/webscraping.html#html-basics))
  - CSS Diner ([flukeout.github.io](https://flukeout.github.io))
  - Selector Gadget ([selectorgadget.com](https://selectorgadget.com/))
- Web Scraping: Introduction, practical exercises
- APIs and Data Formats
- Hands-on Project

# Course Outline Day 2

- Dynamic content, browser automation (`RSelenium`)
- HTTP requests and APIs (`httr`)
  - Custom headers, cookies, authentication
  - When to use `httr` vs. RSelenium
- File management, scheduling, challenges
- Ethics and Legal Aspects: Best practices, Ethical considerations, Legal frameworks
- Outlook
- Hands-on Project

# Textbooks

- Munzert, Simon, Christian Rubba, Peter Meißner, Dominic Nyhuis (2014). Automated Data Collection with R – A Practical Guide to Web Scraping and Text Mining. *John Wiley & Sons*, Chichester. [https://doi.org/10.1002/9781118834732](https://doi.org/10.1002/9781118834732)
- Grolemund, G., & Wickham, H. (2023). R for Data Science (2nd Edition). *O'Reilly Media*. [https://r4ds.hadley.nz/](https://r4ds.hadley.nz/)


# Scraping examples
## Easy
* Wikipedia (useful for networks etc.)
* Parties' press releases (Varies in difficulty)
* Polls (wahlrecht.de)
* Conference programs (EPSA, DVPW)
* Abgeordnetenwatch.de (questions and answers from candidates)
* German Lobby Register (can get very complex)
* Web Search Results (DuckDuckGo)
* News articles

## Medium
* Korean election results (backend, JSON)
* Parliamentary protocols (sometimes as documents or PDFs)
* US live election data from the New York Times (JSON backend)
* Polls (Politico JSON)
* Doctolib appointment availability (JSON)
* List of far-right demonstrations from parliamentary query (parsing PDFs and geocoding)
* Privatization of state owned companies (Treuhand) map (https://treuhandanstalt.online/karte/)

## Difficult
* German Members of Parliament (MPs) websites (parallel scraping/crawling)
* LinkedIn profiles (Python library) (https://pypi.org/project/linkedin-scraper/)
* Air quality sensor data worldwide (encrypted via JavaScript) (https://waqi.info/#/c/3.563/8.145/2.2z)
* Historic shapefiles for Danish parishes (https://dataforsyningen.dk/data/4840)


# Author
- **Cornelius Erfort**  
  Post-doctoral Researcher  
  University of Witten/Herdecke  
  [cornelius.erfort@uni-wh.de](mailto:cornelius.erfort@uni-wh.de)  



