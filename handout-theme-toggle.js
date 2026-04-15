(function () {
  const STORAGE_KEY = "awdc-theme-preference";

  function getSystemTheme() {
    return window.matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
  }

  function getPreferredTheme() {
    const saved = localStorage.getItem(STORAGE_KEY);
    return saved || getSystemTheme();
  }

  function applyTheme(theme) {
    document.documentElement.setAttribute("data-theme", theme);
    const btn = document.getElementById("theme-toggle-btn");
    if (btn) {
      btn.textContent = theme === "dark" ? "Switch to Light" : "Switch to Dark";
      btn.setAttribute("aria-label", btn.textContent);
    }
  }

  function toggleTheme() {
    const current = document.documentElement.getAttribute("data-theme") || getPreferredTheme();
    const next = current === "dark" ? "light" : "dark";
    localStorage.setItem(STORAGE_KEY, next);
    applyTheme(next);
  }

  function ensureButton() {
    if (document.getElementById("theme-toggle-btn")) return;
    const btn = document.createElement("button");
    btn.id = "theme-toggle-btn";
    btn.type = "button";
    btn.className = "theme-toggle-btn";
    btn.addEventListener("click", toggleTheme);
    document.body.appendChild(btn);
  }

  function init() {
    ensureButton();
    applyTheme(getPreferredTheme());
  }

  document.addEventListener("DOMContentLoaded", init);
})();
