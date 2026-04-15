# Syllabus update — 2026

Workshop materials currently reference **April 16–17, 2025** and pre–agent-era scraping pedagogy. This file tracks what to refresh for **2026** and captures content ideas and open questions.

## Checklist

- [ ] Update **dates** and year strings everywhere (`syllabus.md`, `syllabus.tex`, `syllabus.html`, PDF if you regenerate).
- [ ] Revise **learning objectives** to include agent-assisted scraping, skills/MCP, and responsible use (rate limits, ToS, attribution).
- [ ] Add a **module or section on AI & agents** (see suggestions below).
- [ ] Add **practical segment on agent “skills”** (e.g. reusable scraping skills, project conventions, when to use browser MCP vs. scripts).
- [ ] Refresh **example projects** (dead links, sites that changed, new API patterns).
- [ ] Align **R/Python tooling** notes with what you still teach vs. optional (agents may generate either).
- [ ] Add **instructor notes** on limitations: models still hallucinate selectors; always verify in DevTools or with a real fetch.
- [ ] Rebuild **`syllabus.html`** after `syllabus.md` is final.

## Content ideas for 2026

### AI and agents in the scraping workflow

- **What changed:** Large models are now much stronger at proposing **regex**, **CSS selectors**, and **XPath**, and at explaining **HTML structure** from pasted snippets or page source. That lowers the floor for beginners but raises the need for **verification** (tests, spot checks, reproducible scripts).
- **Self-correction:** Agent loops (retry with error logs, run code and fix) can speed up iteration; teach **when that helps** (disposable scripts, one-off pulls) vs. **when it hurts** (opaque pipelines, no version control).
- **Skills / project rules:** Introduce **Agent Skills** (or equivalent) as **repeatable instructions**—project structure, naming, error-handling patterns, and “always check `robots.txt`”—so the agent behaves consistently across sessions.
- **Web scraping skill:** A dedicated skill could cover: inspect → hypothesize selector → fetch in code → respect delays → log failures. Link this to your existing ethics and rate-limiting material.

### Pedagogy shift

- Pair **classic fundamentals** (HTTP, DOM, APIs, async JS) with **“how to supervise an agent”**: writing clear specs, providing HTML excerpts, using DevTools to confirm.
- Optional lab: **pagination** solved by (1) reading network tab, (2) finding JSON API, (3) falling back to DOM—compare reliability and ToS implications.

## Open question: Can agents “see” source vs. network traffic?

**Short answer:** It depends on the **tooling**, not on “Cursor” or “Claude Code” in the abstract.

| What the agent sees | Typical setup |
|---------------------|----------------|
| **Page source / DOM** | You paste HTML, or a **browser MCP** exposes a **snapshot** of the accessible DOM (often not cross-origin iframe content). Some tools give element refs from a live page. |
| **Rendered HTML** | “View source” vs. **Elements** panel differ for SPAs; agents usually see what the tool sends (snapshot/DOM), not always the raw initial HTML. |
| **Network traffic (GET/POST, XHR, fetch)** | **Not by default** in a plain coding chat. With **browser MCP** enabled (e.g. Cursor’s **`cursor-ide-browser`**, including **`browser_network_requests`**), the agent can inspect requests; otherwise paste HAR/cURL or use DevTools manually. |
| **HTTP requests/responses** | **Yes, with MCP:** **Chrome DevTools MCP** (Chrome + DevTools Protocol) or Cursor’s browser MCP—tooling can expose **network requests**, timing, and bodies—so pagination/API discovery becomes teachable as “inspect → reproduce in `httr`/`requests`.” |
| **“Read JavaScript”** | Agents can analyze **script URLs you fetch** or paste, but minified bundles are hard; **network-first** discovery is often faster than reverse-engineering minified JS. |

**Teaching takeaway:** Position **DevTools Network tab** (and optionally **browser MCP**—Chrome DevTools MCP or Cursor’s built-in browser) as the professional path to find **JSON backends** and **pagination parameters**; position **agent-generated selectors** as a draft to **validate**, especially on dynamic sites.

## Chrome DevTools MCP (what it is)

**Chrome DevTools MCP** is an MCP server that connects the assistant in your editor to a **real Chrome** instance using the **Chrome DevTools Protocol** (the same machinery as the Chrome DevTools UI).

**What it typically exposes to the agent**

- **DOM & styles** — structure, computed styles, accessibility
- **Console** — logs and errors
- **Network** — requests and responses, timing (strong fit for **pagination** and **hidden JSON APIs**)
- **Performance / profiling** — CPU, Lighthouse-style checks (exact tools depend on the server version)

**Why it matters for scraping pedagogy:** the model is not limited to a static HTML fetch; it can reason about **live** page behavior and **network** traffic, aligned with how practitioners debug sites.

### vs Cursor’s `cursor-ide-browser`

| | **Chrome DevTools MCP** | **Cursor `cursor-ide-browser`** |
|---|-------------------------|----------------------------------|
| **Role** | Separate MCP install (e.g. npm); attaches to **your Chrome** with DevTools | **Built into Cursor**; browser MCP bundled with the IDE |
| **Setup** | Extra config; students must install/enable | **Settings → MCP**: ensure **`cursor-ide-browser`** is on |
| **Overlap** | DOM, network, console, automation-style workflows | **Snapshots**, clicks, navigation, **`browser_network_requests`**, etc. |

Use both for teaching if you want: **“Chrome DevTools MCP = full DevTools bridge”** vs **“Cursor browser = first-class in-editor browser automation.”** Pick one primary path for the workshop to limit setup friction.

## Suggestions from web research (verify before teaching)

- **Chrome DevTools MCP:** confirm current install and tool list in the [Chrome DevTools MCP](https://github.com/ChromeDevTools/chrome-devtools-mcp) repository before promising specific steps.
- **No MCP:** students should still know **manual** DevTools, **HAR** / **curl** exports, or pasted URLs into chat.

## Participant preparation (for announcements / email)

Copy or adapt:

> **Before the workshop**, please make sure you have:
> 1. **R** and an IDE, ideally **RStudio** (recommended). We will do the hands-on exercises in **R**. (Web scraping is also possible in Python, but we will not cover Python in this workshop.)
> 2. A modern web browser (Firefox or Chrome) with **Developer Tools** available.
>
> If you also want to try AI-assisted workflows, feel free to install **Cursor** (or similar), but it is optional.

## Follow-ups

- [x] **Prereq:** tell participants to install **Cursor (or equivalent)** + **Chrome DevTools MCP** (wording added to `syllabus.md` under **Recommended preparation**).
- [ ] Decide whether the **2026** workshop includes extra **live troubleshooting** time for MCP vs a **demo-only** fallback.
- [ ] Add 1–2 **readings or blog posts** on agent-assisted data collection ethics (if assigning reading).
