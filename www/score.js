document.addEventListener("click", function (e) {
  const el = e.target;

  if (el.tagName !== "INPUT") return;
  if (!el.name || !el.name.startsWith("q")) return;

  const questionId = el.name;
  const value = el.value;
  const note = Number(el.dataset.score || null);

  localStorage.setItem(questionId, JSON.stringify({
    value: value,
    score: note
  }));

  console.log("Stocké :", questionId, "→ valeur =", value, "note =", note);
  Shiny.setInputValue("scores_par_theme", calculerScoresParTheme());

});


function calculerScoresParTheme() {
  const scores = {};

  for (let key in localStorage) {
    if (!key.startsWith("q")) continue;

    try {
      const obj = JSON.parse(localStorage.getItem(key));
      if (!obj || typeof obj.score !== "number") continue;

      const theme = themeMap[key];
      if (!theme) continue;

      if (!scores[theme]) scores[theme] = 0;
      scores[theme] += obj.score;

    } catch (e) {}
  }

  return scores;
}


function getScore(questionId) {
  const raw = localStorage.getItem(questionId);
  if (!raw) return null;
  const obj = JSON.parse(raw);
  return obj.score;
}

