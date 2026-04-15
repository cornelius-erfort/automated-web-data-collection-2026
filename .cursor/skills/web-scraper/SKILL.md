---
name: web-scraper
description: >
  Build reliable web scrapers using a two-phase approach: Discovery (find the
  best data source on a site) then Scraping (generate a single-file Python
  script for scheduled extraction). Use when the user wants to scrape a
  website, extract data from web pages, build a crawler, set up recurring data
  collection, reverse-engineer a site's API, or mentions "scrape", "crawl",
  "extract from site", "web data", "pull data from URL", "scheduled scraping",
  or "monitor a page".
---

# Web Scraper

Build scrapers that survive site redesigns by finding the most stable and efficient
data source first, then generating a clean Python script for recurring extraction.

## Two-Phase Workflow

**Every scraping task MUST go through both phases in order.** Skipping discovery leads
to fragile scrapers that break on the first deploy.

### Phase 1 — Discovery

Goal: find the most efficient and stable way to get the data. Prefer structured endpoints
over DOM parsing. Work down the priority list until you find something that works.

**Before starting discovery, read `discovery-strategies.md`.**

Discovery priority (highest to lowest):

1. **Public/undocumented REST API** — look in XHR/fetch requests, especially on pagination or
   filter actions (page 2, sort, search). Often missed on initial page load.
2. **GraphQL endpoint** — check for `/graphql`, `/gql`, or `__relay` in network requests.
3. **Server-rendered JSON blobs** — `__NEXT_DATA__`, `__NUXT__`, `window.__INITIAL_STATE__`,
   `window.__DATA__`, Remix `__remixContext`, Gatsby `pageContext`, etc.
4. **React Server Components (RSC)** — Next.js App Router sites use RSC instead of `__NEXT_DATA__`.
   Look for `?_rsc=` requests in the network tab. Only parse RSC directly if no cleaner source
   (JSON-LD, API) exists.
5. **CMS/platform API** — WordPress REST (`/wp-json/wp/v2/`), Shopify Storefront API, Drupal JSON:API,
   Contentful, Strapi, Ghost, etc.
6. **Structured data in HTML** — `<script type="application/ld+json">`, microdata, RDFa.
7. **data- attributes** — e.g., `data-product-id`, `data-price`, `data-sku`.
8. **Stable CSS selectors** — semantic HTML elements (`<article>`, `<li>`, `<table>`, `<time>`) over
   class names. Avoid framework-generated classes like `.css-1a2b3c` or `.MuiButton-root`.

Output of discovery: a short report documenting what was found, which approach is recommended, and why.

### Phase 2 — Scraping Script Generation

Goal: produce a single Python script the user can run on a schedule (daily, monthly, etc.) with minimal
dependencies.

**Before generating the script, read `scraping-patterns.md`.**

Key principles:

- **httpx > requests** — async-capable, HTTP/2 support, better defaults.
- **Pure HTTP > headless browser** — always. Only fall back to browser when the data genuinely requires JS execution.
- **curl_cffi** as middle ground — when httpx gets blocked by TLS fingerprinting but full browser is overkill.
- **Playwright** as last resort.

Script requirements:

```text
python scrape.py \
  --output ./data/output.jsonl \       # or .csv, .parquet
  --format jsonl \                      # jsonl | csv | parquet
  --date-range 2024-01-01:2024-01-31 \ # optional
  --page-limit 10 \                     # optional, for pagination
  --connection postgres://...           # optional, write to DB instead
```

The generated script must:

- Use `uv` script metadata header (`# /// script`) for dependency management
- Accept CLI args via `argparse` or `click`
- Support output to file (JSONL default) or DB connection string
- Include rate limiting / polite delays
- Handle pagination automatically
- Log progress to stderr
- Exit with proper codes (0 success, 1 partial, 2 failure)
- Include a `USER_AGENT` constant and `HEADERS` dict at the top for easy tuning
- Be a single file — no package structure needed

## Important Caveats

- Always check `robots.txt` and mention it to the user.
- Note if the site has terms of service that restrict scraping.
- Add appropriate delays between requests (1–3s default for polite scraping).
- For authenticated endpoints, prompt the user for credentials or tokens rather than hardcoding anything.
- If the site uses Cloudflare, Akamai, or similar WAFs, flag this early and adjust the strategy accordingly.

