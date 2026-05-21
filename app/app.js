(function () {
  "use strict";

  if (!window.SCOPE_MOVE_DATA || !window.SCOPE_MOVE_DATA.records) {
    throw new Error("SCOPE-MOVE data bundle was not loaded before app.js.");
  }

  const DATA = window.SCOPE_MOVE_DATA;
  const R = DATA.records;
  const APP_VERSION = "2.0.0";

  const colors = {
    "Sleep": "#CC79A7",
    "Activity type": "#E69F00",
    "Activity intensity": "#56B4E9",
    "Energy expenditure": "#009E73",
    "Traditional": "#0072B2",
    "Non-traditional": "#009E73",
    "Low": "#009E73",
    "High": "#D55E00"
  };

  const covariateDefs = [
    { key: "c_n_classes", label: "Number of behaviour classes (centred)", type: "numeric" },
    { key: "c_k_folds", label: "Number of validation folds (centred)", type: "numeric" },
    { key: "miss_n_classes", label: "Missing number-of-classes indicator", type: "numeric" },
    { key: "miss_k_folds", label: "Missing validation-fold indicator", type: "numeric" },
    { key: "env_re", label: "Test environment", type: "factor" },
    { key: "prot_re", label: "Test protocol", type: "factor" },
    { key: "dom_re", label: "Movement behaviour domain", type: "factor" }
  ];

  const qualityDomainDefs = [
    { key: "Domain 1: Patient Selection/Study Design", short: "Patient selection/study design", code: "D1" },
    { key: "Domain 2: Index Measure", short: "Index measure", code: "D2" },
    { key: "Domain 3: Criterion Measure", short: "Criterion measure", code: "D3" },
    { key: "Domain 4: Flow and Timing", short: "Flow and timing", code: "D4" }
  ];

  const signallingQuestions = buildSignallingQuestions();

  const state = {
    metric: "F1",
    validation: "ALL",
    environment: "ALL",
    protocol: "ALL",
    age: "ALL",
    device: "ALL",
    search: "",
    sort: "estimateDesc",
    poolMethod: "manuscript",
    domains: new Set(),
    methods: new Set(),
    datasetKey: "classifierInput",
    datasetSearch: "",
    qualitySearch: "",
    qualityRisk: "ALL",
    rankingRows: [],
    forestWidthScale: 100,
    forestRowHeight: 24,
    forestViewportHeight: "",
    forestResizeTimer: 0
  };

  const labels = {
    classifierInput: "Classifier extraction input",
    energyInput: "Energy expenditure extraction input",
    studyCharacteristics: "Study characteristics input",
    qualityAssessment: "Quality assessment input",
    appRemlLong2: "Validation performance table"
  };

  const datasetOrder = ["classifierInput", "appRemlLong2", "energyInput", "studyCharacteristics", "qualityAssessment"];

  const doiByValId = new Map(
    R.classifierInput.map((row) => [clean(row.val_id), clean(row.doi)])
  );

  function buildSignallingQuestions() {
    const toolQuestions = DATA.qualityAssessmentTool && Array.isArray(DATA.qualityAssessmentTool.questions)
      ? DATA.qualityAssessmentTool.questions
      : [];
    if (toolQuestions.length) {
      return toolQuestions.map((question) => ({
        key: clean(question.item),
        domain: shortQualityDomain(question.domain),
        label: clean(question.item),
        question: clean(question.question),
        instructions: clean(question.instructions)
      }));
    }
    return [
      { key: "SQ1", domain: "Patient selection/study design", label: "SQ1" },
      { key: "SQ2", domain: "Patient selection/study design", label: "SQ2" },
      { key: "SQ3", domain: "Patient selection/study design", label: "SQ3" },
      { key: "SQ4", domain: "Patient selection/study design", label: "SQ4" },
      { key: "SQ5", domain: "Index measure", label: "SQ5" },
      { key: "SQ6", domain: "Index measure", label: "SQ6" },
      { key: "SQ7", domain: "Criterion measure", label: "SQ7" },
      { key: "SQ8", domain: "Criterion measure", label: "SQ8" },
      { key: "SQ9", domain: "Flow and timing", label: "SQ9" },
      { key: "SQ10", domain: "Flow and timing", label: "SQ10" },
      { key: "SQ11", domain: "Flow and timing", label: "SQ11" }
    ];
  }

  function signallingQuestionText(question) {
    return clean(question.question || question.label || question.key);
  }

  function signallingQuestionTooltip(question) {
    const text = signallingQuestionText(question);
    return text ? `<strong>${escapeHtml(question.key)}</strong><br>${escapeHtml(text)}` : "";
  }

  const performanceRows = R.appRemlLong2.map((row) => {
    const methodGroup = recodeMethodGroup(row.method_group);
    const doi = doiByValId.get(clean(row.val_id)) || "";
    const doiUrl = doi ? "https://doi.org/" + doi.replace(/^https?:\/\/(dx\.)?doi\.org\//i, "") : "";
    return {
      raw: row,
      valId: clean(row.val_id),
      id: clean(row.id),
      study: clean(row.study_id),
      method: clean(row.method),
      methodSub: clean(row.method_sub),
      methodGroup,
      domain: clean(row.domain),
      metric: clean(row.metric),
      estimate: toNumber(row.est),
      ciLow: toNumber(row.ci_l),
      ciHigh: toNumber(row.ci_u),
      variance: toNumber(row.vi_use),
      viSource: clean(row.vi_source),
      sdOrigin: clean(row.sd_origin),
      metricOrigin: clean(row.metric_origin),
      classes: toNumber(row.n_classes),
      folds: toNumber(row.k_folds),
      varianceFold: toNumber(row.vi_fold),
      varianceClass: toNumber(row.vi_class),
      varianceExternal: toNumber(row.vi_ext),
      seUse: toNumber(row.se_use),
      participants: toNumber(row.n_participants),
      environment: clean(row.test_environment),
      protocol: clean(row.test_protocol),
      validation: clean(row.validation_type),
      modelSource: clean(row.is_the_model_published_previously),
      ageGroup: clean(row.age_group),
      device: clean(row.test_device),
      doi,
      doiUrl,
      label: clean(row.row_lab) || [clean(row.study_id), clean(row.method), clean(row.val_id)].filter(Boolean).join(" - ")
    };
  });

  const domains = unique(performanceRows.map((row) => row.domain)).sort(domainSort);
  const methods = ["Traditional", "Non-traditional"];
  domains.forEach((domain) => state.domains.add(domain));
  methods.forEach((method) => state.methods.add(method));

  document.addEventListener("DOMContentLoaded", () => {
    try {
      init();
    } catch (error) {
      renderAppError(error);
    }
  });

  function byId(id) {
    const element = document.getElementById(id);
    if (!element) {
      throw new Error(`Missing required SCOPE-MOVE element: #${id}`);
    }
    return element;
  }

  function renderAppError(error) {
    console.error(error);
    const sourceChip = document.getElementById("sourceChip");
    if (sourceChip) {
      sourceChip.textContent = "SCOPE-MOVE could not finish loading. Reload the page or contact the maintainer.";
    }
  }

  function init() {
    byId("sourceChip").textContent = `SCOPE-MOVE v${APP_VERSION} | Data bundle: ${formatDate(DATA.generatedAt)}`;
    initTheme();
    bindTabs();
    populateFilters();
    bindControls();
    bindNavigationActions();
    renderOverview();
    renderQualityToolGuide();
    renderExplorer();
    renderModelRanking();
    renderLandscape();
    renderQuality();
    renderDataWorkbench();
  }

  function bindTabs() {
    document.querySelectorAll(".tab").forEach((button) => {
      button.addEventListener("click", () => {
        switchView(button.dataset.view);
      });
    });
  }

  function switchView(viewId) {
    document.querySelectorAll(".tab").forEach((tab) => {
      tab.classList.toggle("active", tab.dataset.view === viewId);
    });
    document.querySelectorAll(".view").forEach((view) => {
      view.classList.toggle("active", view.id === viewId);
    });
    window.scrollTo({ top: 0, behavior: "smooth" });
    if (viewId === "explorer") {
      window.requestAnimationFrame(renderExplorer);
    }
  }

  function bindNavigationActions() {
    document.addEventListener("click", (event) => {
      const action = event.target.closest(".nav-action");
      if (!action) return;
      if (action.dataset.metric) {
        state.metric = action.dataset.metric;
        document.getElementById("metricSelect").value = state.metric;
      }
      if (action.dataset.domain) {
        state.domains = new Set([action.dataset.domain]);
        document.querySelectorAll("#domainChecks input").forEach((input) => {
          input.checked = input.value === action.dataset.domain;
        });
      }
      if (action.dataset.dataset) {
        state.datasetKey = action.dataset.dataset;
        document.getElementById("datasetSelect").value = state.datasetKey;
        renderDataWorkbench();
      }
      if (action.dataset.targetView === "explorer") renderExplorer();
      switchView(action.dataset.targetView);
      if (action.dataset.scrollTarget) {
        window.setTimeout(() => {
          const target = document.getElementById(action.dataset.scrollTarget);
          if (target) {
            if ("open" in target) target.open = true;
            target.scrollIntoView({ behavior: "smooth", block: "start" });
          }
        }, 120);
      }
    });
  }

  function initTheme() {
    const saved = localStorage.getItem("scopeMoveTheme");
    const prefersDark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
    const theme = saved || (prefersDark ? "dark" : "light");
    setTheme(theme);
    document.getElementById("themeToggle").addEventListener("click", () => {
      const next = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
      setTheme(next);
      localStorage.setItem("scopeMoveTheme", next);
    });
  }

  function setTheme(theme) {
    document.documentElement.dataset.theme = theme;
    document.getElementById("themeToggle").textContent = theme === "dark" ? "Light" : "Dark";
    document.getElementById("themeToggle").setAttribute("aria-label", theme === "dark" ? "Switch to light mode" : "Switch to dark mode");
  }

  function populateFilters() {
    setOptions("metricSelect", unique(performanceRows.map((row) => row.metric)).sort(metricSort));
    setOptions("validationSelect", unique(performanceRows.map((row) => row.validation)).sort(), true);
    setOptions("environmentSelect", unique(performanceRows.map((row) => row.environment)).sort(), true);
    setOptions("protocolSelect", unique(performanceRows.map((row) => row.protocol)).sort(), true);
    setOptions("ageSelect", unique(flatten(performanceRows.map((row) => tokenize(row.ageGroup)))).sort(), true);
    setOptions("deviceSelect", unique(flatten(performanceRows.map((row) => tokenize(row.device)))).sort(), true);

    document.getElementById("domainChecks").innerHTML = domains.map((domain) => (
      `<label><input type="checkbox" value="${escapeAttr(domain)}" checked> ${escapeHtml(domain)}</label>`
    )).join("");
    document.getElementById("methodChecks").innerHTML = methods.map((method) => (
      `<label><input type="checkbox" value="${escapeAttr(method)}" checked> ${escapeHtml(method)}</label>`
    )).join("");

    const datasetSelect = document.getElementById("datasetSelect");
    datasetSelect.innerHTML = datasetOrder.map((key) => (
      `<option value="${escapeAttr(key)}">${escapeHtml(labels[key] || key)}</option>`
    )).join("");
    document.getElementById("sortSelect").value = state.sort;
    document.getElementById("poolMethodSelect").value = state.poolMethod;
    syncForestSizeInputs();
  }

  function bindControls() {
    const map = [
      ["metricSelect", "metric"],
      ["validationSelect", "validation"],
      ["environmentSelect", "environment"],
      ["protocolSelect", "protocol"],
      ["ageSelect", "age"],
      ["deviceSelect", "device"],
      ["sortSelect", "sort"],
      ["poolMethodSelect", "poolMethod"]
    ];
    map.forEach(([id, key]) => {
      document.getElementById(id).addEventListener("change", (event) => {
        state[key] = event.target.value;
        renderExplorer();
      });
    });

    document.getElementById("performanceSearch").addEventListener("input", (event) => {
      state.search = event.target.value;
      renderExplorer();
    });

    ["forestHeightInput", "forestWidthScaleInput", "forestRowHeightInput"].forEach((id) => {
      document.getElementById(id).addEventListener("input", () => {
        updateForestSizeFromInputs();
        renderExplorer();
      });
    });

    document.getElementById("resetForestSize").addEventListener("click", () => {
      state.forestWidthScale = 100;
      state.forestRowHeight = 24;
      state.forestViewportHeight = "";
      syncForestSizeInputs();
      renderExplorer();
    });

    document.getElementById("domainChecks").addEventListener("change", () => {
      state.domains = checkboxValues("domainChecks");
      renderExplorer();
    });
    document.getElementById("methodChecks").addEventListener("change", () => {
      state.methods = checkboxValues("methodChecks");
      renderExplorer();
    });

    document.getElementById("resetFilters").addEventListener("click", () => {
      state.metric = "F1";
      state.validation = "ALL";
      state.environment = "ALL";
      state.protocol = "ALL";
      state.age = "ALL";
      state.device = "ALL";
      state.search = "";
      state.sort = "estimateDesc";
      state.poolMethod = "manuscript";
      state.domains = new Set(domains);
      state.methods = new Set(methods);
      document.getElementById("metricSelect").value = state.metric;
      document.getElementById("validationSelect").value = "ALL";
      document.getElementById("environmentSelect").value = "ALL";
      document.getElementById("protocolSelect").value = "ALL";
      document.getElementById("ageSelect").value = "ALL";
      document.getElementById("deviceSelect").value = "ALL";
      document.getElementById("performanceSearch").value = "";
      document.getElementById("sortSelect").value = "estimateDesc";
      document.getElementById("poolMethodSelect").value = "manuscript";
      document.querySelectorAll("#domainChecks input, #methodChecks input").forEach((input) => {
        input.checked = true;
      });
      renderExplorer();
    });

    const refreshForest = document.getElementById("refreshForestPlot");
    if (refreshForest) {
      refreshForest.addEventListener("click", renderExplorer);
    }

    window.addEventListener("resize", scheduleForestRefresh);

    document.getElementById("datasetSelect").addEventListener("change", (event) => {
      state.datasetKey = event.target.value;
      renderDataWorkbench();
    });

    document.getElementById("datasetSearch").addEventListener("input", (event) => {
      state.datasetSearch = event.target.value;
      renderDataWorkbench();
    });

    document.getElementById("qualitySearch").addEventListener("input", (event) => {
      state.qualitySearch = event.target.value;
      renderQuality();
    });

    document.getElementById("qualityRiskSelect").addEventListener("change", (event) => {
      state.qualityRisk = event.target.value;
      renderQuality();
    });

    document.getElementById("downloadCsv").addEventListener("click", downloadCurrentDataset);
  }

  function renderOverview() {
    const s = DATA.summaries;

    metricCards("summaryCards", [
      ["Study records", fmt(s.studyRows), `${s.countries} country entries across ${DATA.checks.studyYearRange.min}-${DATA.checks.studyYearRange.max}`],
      ["Validation rows", fmt(s.remlRows), "Primary Tool rows for F1, sensitivity, and specificity"],
      ["Quality rows", fmt(s.qualityRows), "Study-level QUADAS-2 assessment rows"],
      ["Wearable brands", fmt(unique(flatten(R.studyCharacteristics.map((row) => tokenize(row.device_brand)))).length), "Device-brand context for method selection"]
    ]);

    renderPooledOverview();
    renderDocCards("footerDocCards");
    renderStaticDocLinks();
    renderEvidenceMatrix();
  }

  function renderPooledOverview() {
    const routes = [
      ["General user: choose a candidate method", "Start with the Primary Tool, filter by domain, age group, device, and validation type, then inspect above-pooled rows and source papers.", ["Primary Tool", "Quality", "Instructions"], `<button class="ghost-button nav-action primary" data-target-view="explorer">Start comparison</button>`],
      ["Technical user: audit modelling context", "Use the Primary Tool for metric-level evidence, then inspect covariates, model-source fields, validation type, and raw extraction rows.", ["Primary Tool", "Data", "Instructions"], `<button class="ghost-button nav-action" data-target-view="explorer">Inspect validation metadata</button>`],
      ["Evidence mapper: understand the review landscape", "Profile publication timing, countries, device brands, participant characteristics, and quality assessment before interpreting performance.", ["Studies", "Quality", "Primary Tool"], `<button class="ghost-button nav-action" data-target-view="landscape">Map the evidence base</button>`],
      ["Citation or reproducibility check", "Use the footer for software citation and repository metadata, and the Instructions tab for raw-input data dictionary context.", ["Instructions", "Data", "Footer"], `<button class="ghost-button nav-action" data-target-view="instructions">Open field guide</button>`]
    ];

    byId("overviewRouteCards").innerHTML = `<div class="route-flow">${routes.map(([title, body, steps, action]) => `
      <div class="route-card">
        <strong>${escapeHtml(title)}</strong>
        <p>${escapeHtml(body)}</p>
        <div class="flow-steps">${steps.map((step, index) => `<span>${index + 1}. ${escapeHtml(step)}</span>`).join("")}</div>
        ${action}
      </div>`).join("")}</div>`;
  }

  function renderEvidenceMatrix() {
    const metrics = ["F1", "Sensitivity", "Specificity"];
    const html = [
      `<div class="matrix-cell header"></div>`,
      ...metrics.map((metric) => `<div class="matrix-cell header">${escapeHtml(metric)}</div>`)
    ];

    domains.forEach((domain) => {
      html.push(`<div class="matrix-cell header">${escapeHtml(domain)}</div>`);
      metrics.forEach((metric) => {
        const rows = performanceRows.filter((row) => row.domain === domain && row.metric === metric);
        const med = median(rows.map((row) => row.estimate));
        const trad = rows.filter((row) => row.methodGroup === "Traditional").length;
        const non = rows.filter((row) => row.methodGroup === "Non-traditional").length;
        html.push(`<div class="matrix-cell action-cell nav-action" data-target-view="explorer" data-domain="${escapeAttr(domain)}" data-metric="${escapeAttr(metric)}">
          <strong>${fmt(rows.length)}</strong>
          <div class="tag-row">
            <span class="pill">Median ${pct(med, 1)}</span>
            <span class="pill">Traditional ${trad}</span>
            <span class="pill">Non-trad ${non}</span>
          </div>
        </div>`);
      });
    });
    byId("evidenceMatrix").innerHTML = `<div class="matrix">${html.join("")}</div>`;
  }

  function renderExplorer() {
    const rows = filteredPerformanceRows();
    const pooled = computePooled(rows, state.poolMethod);
    metricCards("performanceCards", performanceSummaryCards(rows, pooled));

    byId("pooledLineNote").textContent = pooled ? pooled.source : "At least 3 validation rows are needed for a pooled line";
    renderForestLegend();
    renderPoolDetails(rows, pooled);
    renderForestChart(rows, pooled);
    renderSelectedTable(rows, pooled);
  }

  function performanceSummaryCards(rows, pooled) {
    const studies = unique(rows.map((row) => row.study)).length;
    const participants = rows.map((row) => row.participants).filter(Number.isFinite).reduce((a, b) => a + b, 0);
    const estimates = rows.map((row) => row.estimate).filter(Number.isFinite);
    const abovePooled = pooled ? rows.filter((row) => Number.isFinite(row.estimate) && row.estimate > pooled.est).length : 0;
    const externalRows = rows.filter(isExternalValidation).length;
    return [
      ["Rows", fmt(rows.length), `${studies} studies in current selection`],
      ["Participants", fmt(participants), "Summed across selected metric rows"],
      ["Median estimate", pct(median(estimates), 1), iqrText(estimates)],
      ["Min to max", `${pct(min(estimates), 1)}-${pct(max(estimates), 1)}`, "Displayed on a 0-100 percent scale", "small-value"],
      ["Pooled line", pooled ? pct(pooled.est, 1) : "n.a.", pooled ? pooled.source : "Shown when the filtered view has at least 3 rows"],
      ["Above pooled", pooled ? fmt(abovePooled) : "n.a.", "Highlighted rows to investigate first"],
      ["External validations", fmt(externalRows), "Marked with a diamond and EXT tag"]
    ];
  }

  function filteredPerformanceRows() {
    const search = state.search.trim().toLowerCase();
    const rows = performanceRows.filter((row) => {
      if (row.metric !== state.metric) return false;
      if (!state.domains.has(row.domain)) return false;
      if (!state.methods.has(row.methodGroup)) return false;
      if (state.validation !== "ALL" && row.validation !== state.validation) return false;
      if (state.environment !== "ALL" && row.environment !== state.environment) return false;
      if (state.protocol !== "ALL" && row.protocol !== state.protocol) return false;
      if (state.age !== "ALL" && !tokenize(row.ageGroup).includes(state.age)) return false;
      if (state.device !== "ALL" && !tokenize(row.device).includes(state.device)) return false;
      if (search) {
        const haystack = [row.study, row.method, row.methodSub, row.domain, row.device, row.ageGroup, row.validation, row.environment, row.protocol, row.valId].join(" ").toLowerCase();
        if (!haystack.includes(search)) return false;
      }
      return true;
    });

    rows.sort((a, b) => {
      if (state.sort === "estimateDesc") return b.estimate - a.estimate;
      if (state.sort === "study") return a.study.localeCompare(b.study) || a.estimate - b.estimate;
      if (state.sort === "domain") return domainSort(a.domain, b.domain) || a.estimate - b.estimate;
      return a.estimate - b.estimate;
    });
    return rows;
  }

  function computePooled(rows, method) {
    const usable = rows.filter((row) => Number.isFinite(row.estimate));
    if (usable.length < 3) return null;

    if (method === "simple") {
      const effects = usable.map((row) => ({
        yi: Number.isFinite(toNumber(row.raw.yi)) ? toNumber(row.raw.yi) : logit(row.estimate),
        vi: Math.max(toNumber(row.raw.vi_use), 1e-6)
      })).filter((row) => Number.isFinite(row.yi) && Number.isFinite(row.vi));
      if (effects.length < 3) return null;
      const fixedWeights = effects.map((row) => 1 / row.vi);
      const fixedMean = weightedAverage(effects.map((row) => row.yi), fixedWeights);
      const q = effects.reduce((sum, row, index) => sum + fixedWeights[index] * Math.pow(row.yi - fixedMean, 2), 0);
      const sumW = fixedWeights.reduce((a, b) => a + b, 0);
      const sumW2 = fixedWeights.reduce((sum, weight) => sum + weight * weight, 0);
      const c = sumW - (sumW2 / sumW);
      const tau2 = c > 0 ? Math.max(0, (q - (effects.length - 1)) / c) : 0;
      const randomWeights = effects.map((row) => 1 / (row.vi + tau2));
      const eta = weightedAverage(effects.map((row) => row.yi), randomWeights);
      const se = Math.sqrt(1 / randomWeights.reduce((a, b) => a + b, 0));
      return {
        est: invLogit(eta),
        low: invLogit(eta - 1.96 * se),
        high: invLogit(eta + 1.96 * se),
        source: "Simple random-effects line, no covariates"
      };
    }

    if (method === "manuscript") {
      return computeManuscriptPooled(usable);
    }

    return participantWeightedPooled(usable, "Participant-weighted pooled estimate");
  }

  function computeManuscriptPooled(rows) {
    const covariates = covariateInfo(rows);
    const design = designMatrix(rows, covariates.kept);
    const effects = rows.map((row, index) => ({
      y: Number.isFinite(toNumber(row.raw.yi)) ? toNumber(row.raw.yi) : logit(row.estimate),
      variance: Math.max(toNumber(row.raw.vi_use), 1e-6),
      x: design.matrix[index],
      row
    })).filter((item) => Number.isFinite(item.y) && Number.isFinite(item.variance) && item.x);

    if (effects.length < 3 || !design.columns.length) {
      const fallback = participantWeightedPooled(rows, "Manuscript covariate pooled line");
      fallback.covariates = covariates;
      fallback.note = "Covariate fit unavailable for this subset; displayed as participant-weighted orientation.";
      return fallback;
    }

    const fit = weightedLinearRegression(
      effects.map((item) => item.x),
      effects.map((item) => item.y),
      effects.map((item) => 1 / item.variance)
    );

    if (!fit) {
      const fallback = participantWeightedPooled(rows, "Manuscript covariate pooled line");
      fallback.covariates = covariates;
      fallback.note = "Covariate fit unavailable for this subset; displayed as participant-weighted orientation.";
      return fallback;
    }

    const probabilities = effects.map((item) => clamp(invLogit(dot(item.x, fit.beta)), 1e-6, 1 - 1e-6));
    const participantWeights = effects.map((item) => Number.isFinite(item.row.participants) && item.row.participants > 0 ? item.row.participants : 1);
    const totalWeight = participantWeights.reduce((a, b) => a + b, 0);
    const normalizedWeights = participantWeights.map((weight) => weight / totalWeight);
    const est = weightedAverage(probabilities, participantWeights);
    const gradient = new Array(fit.beta.length).fill(0);
    effects.forEach((item, index) => {
      const p = probabilities[index];
      const scalar = normalizedWeights[index] * p * (1 - p);
      item.x.forEach((value, col) => {
        gradient[col] += scalar * value;
      });
    });
    const variance = quadraticForm(gradient, fit.covariance);
    const se = Number.isFinite(variance) && variance > 0 ? Math.sqrt(variance) : NaN;

    return {
      est,
      low: Number.isFinite(se) ? clamp(est - 1.96 * se, 0, 1) : NaN,
      high: Number.isFinite(se) ? clamp(est + 1.96 * se, 0, 1) : NaN,
      source: "Manuscript covariate pooled line",
      covariates,
      designColumns: design.columns,
      note: "Exploratory browser calculation using the manuscript covariate set available in the validation table."
    };
  }

  function participantWeightedPooled(rows, source) {
    const usable = rows.filter((row) => Number.isFinite(row.estimate));
    const weights = usable.map((row) => Number.isFinite(row.participants) && row.participants > 0 ? row.participants : 1);
    return {
      est: weightedAverage(usable.map((row) => row.estimate), weights),
      low: NaN,
      high: NaN,
      source
    };
  }

  function covariateInfo(rows) {
    const entries = covariateDefs.map((def) => {
      const values = unique(rows.map((row) => clean(row.raw[def.key])).filter((value) => value !== ""));
      const numericValues = rows.map((row) => toNumber(row.raw[def.key])).filter(Number.isFinite);
      const varied = def.type === "numeric" ? unique(numericValues.map((value) => String(value))).length > 1 : values.length > 1;
      return {
        key: def.key,
        label: def.label,
        type: def.type,
        values,
        varied,
        reason: varied ? "" : "constant or unavailable in current filter"
      };
    });
    return {
      kept: entries.filter((entry) => entry.varied),
      dropped: entries.filter((entry) => !entry.varied)
    };
  }

  function designMatrix(rows, keptCovariates) {
    const columns = [{ label: "Intercept", value: () => 1 }];
    keptCovariates.forEach((covariate) => {
      if (covariate.type === "factor") {
        const levels = covariate.values.slice().sort();
        levels.slice(1).forEach((level) => {
          columns.push({
            label: `${covariate.label}: ${level}`,
            value: (row) => clean(row.raw[covariate.key]) === level ? 1 : 0
          });
        });
      } else {
        columns.push({
          label: covariate.label,
          value: (row) => {
            const value = toNumber(row.raw[covariate.key]);
            return Number.isFinite(value) ? value : 0;
          }
        });
      }
    });
    return {
      columns: columns.map((column) => column.label),
      matrix: rows.map((row) => columns.map((column) => column.value(row)))
    };
  }

  function weightedLinearRegression(xRows, y, weights) {
    const cols = xRows[0] ? xRows[0].length : 0;
    if (!cols) return null;
    const xtwx = Array.from({ length: cols }, () => new Array(cols).fill(0));
    const xtwy = new Array(cols).fill(0);
    xRows.forEach((row, rowIndex) => {
      const weight = Number.isFinite(weights[rowIndex]) && weights[rowIndex] > 0 ? weights[rowIndex] : 1;
      row.forEach((xj, j) => {
        xtwy[j] += weight * xj * y[rowIndex];
        row.forEach((xk, k) => {
          xtwx[j][k] += weight * xj * xk;
        });
      });
    });
    const ridge = 1e-8;
    for (let i = 0; i < cols; i += 1) xtwx[i][i] += ridge;
    const inverse = invertMatrix(xtwx);
    if (!inverse) return null;
    return {
      beta: multiplyMatrixVector(inverse, xtwy),
      covariance: inverse
    };
  }

  function invertMatrix(matrix) {
    const n = matrix.length;
    const a = matrix.map((row, index) => row.slice().concat(
      Array.from({ length: n }, (_, col) => index === col ? 1 : 0)
    ));
    for (let col = 0; col < n; col += 1) {
      let pivot = col;
      for (let row = col + 1; row < n; row += 1) {
        if (Math.abs(a[row][col]) > Math.abs(a[pivot][col])) pivot = row;
      }
      if (Math.abs(a[pivot][col]) < 1e-12) return null;
      if (pivot !== col) [a[pivot], a[col]] = [a[col], a[pivot]];
      const divisor = a[col][col];
      for (let j = 0; j < 2 * n; j += 1) a[col][j] /= divisor;
      for (let row = 0; row < n; row += 1) {
        if (row === col) continue;
        const factor = a[row][col];
        for (let j = 0; j < 2 * n; j += 1) a[row][j] -= factor * a[col][j];
      }
    }
    return a.map((row) => row.slice(n));
  }

  function multiplyMatrixVector(matrix, vector) {
    return matrix.map((row) => dot(row, vector));
  }

  function quadraticForm(vector, matrix) {
    const mv = multiplyMatrixVector(matrix, vector);
    return dot(vector, mv);
  }

  function dot(a, b) {
    return a.reduce((sum, value, index) => sum + value * b[index], 0);
  }

  function renderForestLegend() {
    const domainKeys = domains.map((domain) => (
      `<span class="legend-key"><span class="swatch" style="background:${colors[domain] || "#777"}"></span>${escapeHtml(domain)}</span>`
    ));
    const statusKeys = [
      `<span class="legend-key"><span class="legend-line"></span>Pooled line</span>`,
      `<span class="legend-key"><span class="legend-line horizontal"></span>Above/below split</span>`,
      `<span class="legend-key"><span class="above-key"></span>Exceeds pooled estimate</span>`,
      `<span class="legend-key"><span class="diamond-key neutral"></span>External validation</span>`
    ];
    document.getElementById("forestLegend").innerHTML = domainKeys.concat(statusKeys).join("");
  }

  function renderPoolDetails(rows, pooled) {
    const target = document.getElementById("poolDetails");
    if (!target) return;
    if (!pooled) {
      target.innerHTML = `<div class="pool-details-card">At least 3 validation rows are needed for a pooled line.</div>`;
      return;
    }

    const above = rows.filter((row) => Number.isFinite(row.estimate) && row.estimate > pooled.est).length;
    const external = rows.filter(isExternalValidation).length;
    if (state.poolMethod !== "manuscript") {
      target.innerHTML = `<div class="pool-details-card">
        <strong>${escapeHtml(pooled.source)}</strong>
        <p>The vertical dashed line is computed without covariates. ${fmt(above)} validation rows exceed the pooled estimate; ${fmt(external)} selected rows are external validations.</p>
      </div>`;
      return;
    }

    const covariates = pooled.covariates || covariateInfo(rows);
    const kept = covariates.kept.map((item) => item.label).join("; ") || "Intercept only";
    const dropped = covariates.dropped.map((item) => `${item.label} (${item.reason})`).join("; ") || "None";
    const ciText = Number.isFinite(pooled.low) ? `${pct(pooled.est, 1)} (${pct(pooled.low, 1)} to ${pct(pooled.high, 1)})` : `${pct(pooled.est, 1)} (CI n.a.)`;

    target.innerHTML = `<div class="pool-details-grid">
      <div class="pool-details-card">
        <strong>Manuscript pooled estimate</strong>
        <p>${ciText}. Rows above this line are highlighted and should be inspected with protocol, population, device, and validation type.</p>
      </div>
      <div class="pool-details-card">
        <strong>Covariates retained</strong>
        <p>${escapeHtml(kept)}</p>
      </div>
      <div class="pool-details-card">
        <strong>Dropped covariates</strong>
        <p>${escapeHtml(dropped)}</p>
      </div>
      <div class="pool-details-card">
        <strong>Validation signal</strong>
        <p>${fmt(above)} rows exceed the pooled estimate; ${fmt(external)} selected rows are external validations.</p>
      </div>
    </div>
    <p class="pool-footnote">${escapeHtml(pooled.note || "Covariates that are constant or effectively unavailable in the filtered subset are removed from the pooled-line calculation.")}</p>`;
  }

  function scheduleForestRefresh() {
    window.clearTimeout(state.forestResizeTimer);
    state.forestResizeTimer = window.setTimeout(() => {
      const explorer = document.getElementById("explorer");
      if (explorer && explorer.classList.contains("active")) {
        renderExplorer();
      }
    }, 140);
  }

  function syncForestSizeInputs() {
    const heightInput = document.getElementById("forestHeightInput");
    const widthInput = document.getElementById("forestWidthScaleInput");
    const rowInput = document.getElementById("forestRowHeightInput");
    if (!heightInput || !widthInput || !rowInput) return;
    heightInput.value = state.forestViewportHeight || "";
    widthInput.value = String(state.forestWidthScale || 100);
    rowInput.value = String(state.forestRowHeight || 24);
  }

  function updateForestSizeFromInputs() {
    const heightText = document.getElementById("forestHeightInput").value.trim();
    const width = Number(document.getElementById("forestWidthScaleInput").value);
    const rowHeight = Number(document.getElementById("forestRowHeightInput").value);
    const height = Number(heightText);

    state.forestViewportHeight = heightText && Number.isFinite(height) ? Math.max(180, height) : "";
    state.forestWidthScale = Number.isFinite(width) ? Math.max(40, width) : 100;
    state.forestRowHeight = Number.isFinite(rowHeight) ? Math.max(12, rowHeight) : 24;
  }

  function forestLayout(container, rowCount) {
    const panel = container.closest(".panel");
    const workspace = container.closest(".workspace-panel");
    const measuredWidth = Math.floor(
      container.clientWidth ||
      (panel && panel.clientWidth) ||
      (workspace && workspace.clientWidth) ||
      document.documentElement.clientWidth - 64 ||
      1180
    );
    const width = Math.max(420, Math.round(measuredWidth * (state.forestWidthScale / 100)));
    const compact = width < 960;
    const left = compact ? clamp(Math.round(width * 0.32), 170, 280) : clamp(Math.round(width * 0.28), 290, 520);
    const right = Math.max(left + 260, width - (compact ? 34 : 48));
    const top = 28;
    const rowH = Math.max(12, state.forestRowHeight || (compact ? 23 : 24));
    const height = top + rowCount * rowH + 32;

    return {
      width,
      left,
      right,
      top,
      rowH,
      height,
      labelLength: Math.round(clamp(left / 7.8, 18, 72))
    };
  }

  function setForestViewportHeight(container, contentHeight) {
    const customHeight = Number(state.forestViewportHeight);
    if (Number.isFinite(customHeight) && customHeight >= 180) {
      container.style.height = `${customHeight}px`;
      return;
    }

    const viewport = window.innerHeight || document.documentElement.clientHeight || 760;
    const target = clamp(Math.round(viewport * 0.62), 340, 660);
    const fitted = Number.isFinite(contentHeight) ? Math.min(contentHeight + 2, target) : target;
    container.style.height = `${Math.max(220, fitted)}px`;
  }

  function renderForestChart(rows, pooled) {
    const container = document.getElementById("forestChart");
    if (!rows.length) {
      container.innerHTML = `<div class="empty-state">No validation rows match the current filter set.</div>`;
      return;
    }

    const { width, left, right, top, rowH, height, labelLength } = forestLayout(container, rows.length);
    setForestViewportHeight(container, height);
    const x = (value) => left + Math.max(0, Math.min(1, value)) * (right - left);
    const ticks = [0, 0.25, 0.5, 0.75, 1];
    const exceedFlags = rows.map((row) => pooled && Number.isFinite(row.estimate) && row.estimate > pooled.est);
    const firstNotExceed = exceedFlags.findIndex((flag) => !flag);
    const hasCleanSplit = pooled && firstNotExceed > 0 && firstNotExceed < rows.length && exceedFlags.slice(firstNotExceed).every((flag) => !flag);
    const splitY = hasCleanSplit ? top + firstNotExceed * rowH - rowH / 2 : null;

    const axis = ticks.map((tick) => `
      <g class="axis">
        <line x1="${x(tick)}" x2="${x(tick)}" y1="${top - 12}" y2="${height - 26}" stroke="var(--line)" />
        <text x="${x(tick)}" y="${height - 9}" text-anchor="middle">${Math.round(tick * 100)}%</text>
      </g>
    `).join("");

    const pooledSvg = pooled ? `
      <line x1="${x(pooled.est)}" x2="${x(pooled.est)}" y1="${top - 14}" y2="${height - 28}" stroke="var(--ink)" stroke-dasharray="6 5" stroke-width="2" />
      <text x="${x(pooled.est) + 7}" y="${top - 18}" fill="var(--ink)" font-size="12" font-weight="850">Pooled ${escapeHtml(pct(pooled.est, 1))}</text>
    ` : "";

    const splitSvg = splitY ? `
      <line x1="8" x2="${right}" y1="${splitY}" y2="${splitY}" stroke="var(--ink)" stroke-dasharray="4 5" stroke-width="1.4" />
      <text class="forest-split-label" x="${left}" y="${splitY - 6}">Rows above exceed pooled estimate</text>
    ` : "";

    const rowSvg = rows.map((row, index) => {
      const y = top + index * rowH;
      const low = Math.min(row.ciLow, row.estimate, row.ciHigh);
      const high = Math.max(row.ciLow, row.estimate, row.ciHigh);
      const label = truncate(row.label, labelLength);
      const exceeds = pooled && Number.isFinite(row.estimate) && row.estimate > pooled.est;
      const external = isExternalValidation(row);
      const tooltip = [
        `<strong>${escapeHtml(row.study)}</strong>`,
        `${escapeHtml(row.metric)}: ${pct(row.estimate, 1)} (${pct(low, 1)} to ${pct(high, 1)})`,
        pooled ? `${exceeds ? "Above" : "At/below"} pooled estimate (${pct(pooled.est, 1)})` : "",
        external ? "External validation" : "",
        external ? `Model source: ${escapeHtml(modelSourceText(row))}` : "",
        `${escapeHtml(row.domain)} | ${escapeHtml(row.methodGroup)} | ${escapeHtml(row.method)}`,
        `${escapeHtml(row.validation)}; ${escapeHtml(row.environment)}; ${escapeHtml(row.protocol)}`,
        `Age: ${escapeHtml(row.ageGroup || "n.r.")}; device: ${escapeHtml(row.device || "n.r.")}`,
        `Participants: ${Number.isFinite(row.participants) ? fmt(row.participants) : "n.r."}`,
        varianceTooltipText(row),
        row.doiUrl ? `<span class="doi-callout">Click row to open DOI</span>` : ""
      ].filter(Boolean).join("<br>");
      const color = colors[row.domain] || "#555";
      const rowFill = exceeds ? color : (index % 2 ? "var(--row-even)" : "var(--row-odd)");
      const rowOpacity = exceeds ? "0.14" : "1";
      const markerX = x(row.estimate);
      const marker = external
        ? `<polygon points="${markerX},${y - 6.2} ${markerX + 6.2},${y} ${markerX},${y + 6.2} ${markerX - 6.2},${y}" fill="${color}" stroke="var(--ink)" stroke-width="1.6" />`
        : `<circle cx="${markerX}" cy="${y}" r="${exceeds ? "5.2" : "4.6"}" fill="${color}" stroke="var(--panel)" stroke-width="1.5" />`;
      const aboveTag = exceeds ? `<text class="forest-tag above" x="${left - 86}" y="${y + 4}">ABOVE</text>` : "";
      const externalTag = external ? `<text class="forest-tag external" x="${left - 36}" y="${y + 4}">EXT</text>` : "";
      return `
        <g class="row-hit" data-tooltip="${escapeAttr(tooltip)}" data-url="${escapeAttr(row.doiUrl)}">
          <line x1="8" x2="${right}" y1="${y}" y2="${y}" stroke="${rowFill}" stroke-opacity="${rowOpacity}" stroke-width="${rowH}" />
          <text class="forest-label" x="14" y="${y + 4}">${escapeHtml(label)}</text>
          ${aboveTag}
          ${externalTag}
          <line x1="${x(low)}" x2="${x(high)}" y1="${y}" y2="${y}" stroke="${color}" stroke-width="2" />
          ${marker}
        </g>`;
    }).join("");

    container.innerHTML = `
      <svg class="forest-svg" viewBox="0 0 ${width} ${height}" width="${width}" height="${height}" preserveAspectRatio="xMinYMin meet" role="img" aria-label="Forest plot of selected validation rows">
        <rect width="${width}" height="${height}" fill="var(--panel-2)"></rect>
        <text class="forest-title" x="${left}" y="18" font-size="12" font-weight="850">Estimate with computed 95% CI</text>
        ${axis}
        ${pooledSvg}
        ${splitSvg}
        ${rowSvg}
      </svg>`;

    bindTooltips(container);
  }

  function renderSelectedTable(rows, pooled) {
    document.getElementById("selectedRowCount").textContent = `${rows.length} rows`;
    const tableRows = rows.map((row) => ({
      Study: row.study,
      Metric: row.metric,
      Estimate: pct(row.estimate, 1),
      "95% CI": `${pct(Math.min(row.ciLow, row.estimate, row.ciHigh), 1)} to ${pct(Math.max(row.ciLow, row.estimate, row.ciHigh), 1)}`,
      "Pooled comparison": pooled ? (row.estimate > pooled.est ? "Above pooled" : "At/below pooled") : "",
      "External validation": isExternalValidation(row) ? "External" : "",
      "Model source / prior publication": modelSourceText(row),
      Domain: row.domain,
      Group: row.methodGroup,
      Method: row.method,
      Validation: row.validation,
      Environment: row.environment,
      Protocol: row.protocol,
      Participants: Number.isFinite(row.participants) ? fmt(row.participants) : "",
      DOI: row.doi
    }));
    renderTable("selectedTable", tableRows, 220);
  }

  function varianceTooltipText(row) {
    const parts = [
      `<strong>Variance construction</strong>`,
      `Metric origin: ${escapeHtml(row.metricOrigin || "n.r.")}; SD origin: ${escapeHtml(row.sdOrigin || "n.r.")}`,
      `Classes: ${Number.isFinite(row.classes) ? fmt(row.classes) : "n.a."}; folds: ${Number.isFinite(row.folds) ? fmt(row.folds) : "n.a."}`,
      `Selected variance: ${formatVariance(row.variance)} (${escapeHtml(row.viSource || "source n.r.")}); SE: ${formatVariance(row.seUse)}`,
      `Components: CV folds ${formatVariance(row.varianceFold)} | classes ${formatVariance(row.varianceClass)} | external test ${formatVariance(row.varianceExternal)}`
    ];
    return parts.join("<br>");
  }

  function formatVariance(value) {
    return Number.isFinite(value) ? value.toPrecision(4) : "n.a.";
  }

  function renderModelRanking() {
    const rows = buildModelRankingRows();
    state.rankingRows = rows;
    const totalExternal = rows.reduce((sum, row) => sum + row.externalValidations, 0);
    const movementRows = rows.filter((row) => row.family === "movement");
    const sleepRows = rows.filter((row) => row.family === "sleep");
    const topMovement = movementRows[0];
    const topSleep = sleepRows[0];

    metricCards("modelRankingCards", [
      ["Eligible models", fmt(rows.length), "Named in at least one external-validation source field"],
      ["External validations", fmt(totalExternal), "Assigned to harmonised model identities"],
      ["Top activity model", topMovement ? topMovement.model : "n.a.", topMovement ? `Activity intensity/type score ${topMovement.score.toFixed(1)}` : "No eligible activity model rows", "small-value"],
      ["Top sleep model", topSleep ? topSleep.model : "n.a.", topSleep ? `Sleep score ${topSleep.score.toFixed(1)}` : "No eligible sleep model rows", "small-value"],
      ["Performance rule", "F1 first", "Fallback to mean sensitivity/specificity when F1 is not reported"]
    ]);

    document.getElementById("modelRankingCount").textContent = `${fmt(rows.length)} eligible models`;
    renderTable("modelRankingTable", rows.map((row) => ({
      Rank: row.rank,
      Model: row.model,
      "Composite score": row.score.toFixed(1),
      "External validations": row.externalValidations,
      "Cross-validations": row.crossValidations,
      "Mean performance": pct(row.meanPerformance, 1),
      "Different devices": row.deviceCount,
      Domains: row.domains.join("; "),
      "Linked validation rows": row.validationRows
    })), 200);

    document.getElementById("modelRankingDetails").innerHTML = `
      <div class="ranking-column movement">
        <h4>Activity intensity and activity type</h4>
        ${rankingCardsMarkup(movementRows.slice(0, 8))}
      </div>
      <div class="ranking-column sleep">
        <h4>Sleep-wake</h4>
        ${rankingCardsMarkup(sleepRows.slice(0, 8))}
      </div>`;
  }

  function rankingCardsMarkup(rows) {
    return rows.length ? rows.map((row) => `
      <details class="ranking-card">
        <summary>
          <span><strong>${row.rank}. ${escapeHtml(row.model)}</strong><em>${pct(row.meanPerformance, 1)} mean performance across ${fmt(row.validationRows)} linked rows</em></span>
          <span class="quality-card-score">${row.score.toFixed(1)}</span>
        </summary>
        <div class="quality-card-body">
          <div class="risk-chip-row">
            <span class="pill">${fmt(row.externalValidations)} external</span>
            <span class="pill">${fmt(row.crossValidations)} cross-validation</span>
            <span class="pill">${fmt(row.deviceCount)} devices</span>
            <span class="pill">${fmt(row.ageGroups.length)} age groups</span>
          </div>
          <div class="ranking-meta-grid">
            <div><strong>Devices</strong><p>${escapeHtml(listText(row.devices, 10))}</p></div>
            <div><strong>Age groups</strong><p>${escapeHtml(listText(row.ageGroups, 10))}</p></div>
            <div><strong>Validation types</strong><p>${escapeHtml(listText(row.validationTypes, 10))}</p></div>
            <div><strong>Externally validating studies</strong><p>${escapeHtml(listText(row.externalStudies, 12))}</p></div>
          </div>
        </div>
      </details>`).join("") : `<div class="empty-state">No eligible models in this domain family.</div>`;
  }

  function listText(values, limit) {
    const items = unique((values || []).map(clean).filter((value) => value && !isNotReported(value)));
    if (!items.length) return "n.r.";
    const shown = items.slice(0, limit);
    const remaining = items.length - shown.length;
    return remaining > 0 ? `${shown.join("; ")}; +${fmt(remaining)} more` : shown.join("; ");
  }

  function buildModelRankingRows() {
    const classifierRows = R.classifierInput;
    const seedMap = new Map();
    classifierRows
      .filter((row) => isExternalValidationText(row.validation_type))
      .forEach((row) => {
        modelSourceTokens(row["Is the model published previously?"]).forEach((token) => {
          if (!seedMap.has(token.key)) {
            seedMap.set(token.key, { key: token.key, model: token.label });
          }
        });
      });

    const assignments = new Map(Array.from(seedMap.keys()).map((key) => [key, []]));
    classifierRows
      .filter((row) => isRankingValidation(row.validation_type))
      .forEach((row) => {
        const rowKeys = new Set(modelSourceTokens(row["Is the model published previously?"]).map((token) => token.key));
        const studyKey = authorYearKey(row.study);
        if (studyKey) rowKeys.add(studyKey);
        rowKeys.forEach((key) => {
          if (assignments.has(key)) assignments.get(key).push(row);
        });
      });

    const summaries = Array.from(seedMap.values()).map((seed) => {
      const linkedRows = uniqueByKey(assignments.get(seed.key) || [], (row) => clean(row.val_id) || [row.study, row.method, row.validation_type].join("|"));
      const externalRows = linkedRows.filter((row) => isExternalValidationText(row.validation_type));
      if (!externalRows.length) return null;
      const crossRows = linkedRows.filter((row) => /cross/i.test(clean(row.validation_type)));
      const performanceValues = linkedRows.map(rowPerformanceValue).filter(Number.isFinite);
      const devices = unique(flatten(linkedRows.map((row) => tokenize(row.test_device).filter((value) => !isNotReported(value))))).sort();
      const ageGroups = unique(flatten(linkedRows.map((row) => tokenize(row.age_group).filter((value) => !isNotReported(value))))).sort();
      const validationTypes = unique(linkedRows.map((row) => clean(row.validation_type)).filter(Boolean)).sort();
      const externalStudies = unique(externalRows.map((row) => clean(row.study)).filter(Boolean)).sort();
      const domainsForModel = unique(linkedRows.map((row) => clean(row.movement_behaviour))).sort(domainSort);
      const methodsForModel = unique(linkedRows.map((row) => [clean(row.method), clean(row.method_sub)].filter(Boolean).join(" / "))).slice(0, 5);
      const family = modelDomainFamily(domainsForModel);
      return {
        key: seed.key,
        model: seed.model,
        family,
        validationRows: linkedRows.length,
        externalValidations: externalRows.length,
        crossValidations: crossRows.length,
        meanPerformance: performanceValues.length ? weightedAverage(performanceValues, performanceValues.map(() => 1)) : NaN,
        deviceCount: devices.length,
        devices,
        ageGroups,
        validationTypes,
        externalStudies,
        domains: domainsForModel,
        methods: methodsForModel
      };
    }).filter(Boolean);

    const maxExternal = Math.max(1, ...summaries.map((row) => row.externalValidations));
    const maxDevices = Math.max(1, ...summaries.map((row) => row.deviceCount));
    summaries.forEach((row) => {
      const externalComponent = row.externalValidations / maxExternal;
      const performanceComponent = Number.isFinite(row.meanPerformance) ? row.meanPerformance : 0;
      const deviceComponent = row.deviceCount / maxDevices;
      row.score = 100 * (externalComponent + performanceComponent + deviceComponent) / 3;
    });

    return summaries
      .sort((a, b) => b.score - a.score || b.externalValidations - a.externalValidations || b.meanPerformance - a.meanPerformance || a.model.localeCompare(b.model))
      .map((row, index) => ({ ...row, rank: index + 1 }));
  }

  function rowPerformanceValue(row) {
    const f1 = metricPercent(row.f1_mean);
    if (Number.isFinite(f1)) return f1;
    const sens = metricPercent(row.sens_mean);
    const spec = metricPercent(row.spec_mean);
    const values = [sens, spec].filter(Number.isFinite);
    return values.length ? weightedAverage(values, values.map(() => 1)) : NaN;
  }

  function metricPercent(value) {
    const number = toNumber(value);
    if (!Number.isFinite(number)) return NaN;
    return number > 1 ? number / 100 : number;
  }

  function isRankingValidation(value) {
    const text = clean(value);
    return /external/i.test(text) || /cross/i.test(text);
  }

  function isExternalValidationText(value) {
    return /external/i.test(clean(value));
  }

  function modelSourceTokens(value) {
    let text = clean(value);
    if (!text || /^no$/i.test(text) || /^n\.?r\.?$/i.test(text)) return [];
    text = text
      .replace(/^yes\s*[-:]\s*/i, "")
      .replace(/vanHees/gi, "van Hees")
      .replace(/algorthm/gi, "algorithm")
      .replace(/\bpossibly others\b/gi, "")
      .replace(/\bopen-source\b/gi, "");

    const tokens = new Map();
    let currentAuthor = "";
    text.split(/[;,]/).forEach((rawSegment) => {
      let segment = clean(rawSegment).replace(/\s+/g, " ");
      if (!segment) return;
      const leadingYear = segment.match(/^(\d{4}[a-z]?)(.*)$/i);
      if (leadingYear && currentAuthor) {
        addModelToken(tokens, currentAuthor, leadingYear[1], leadingYear[2]);
        segment = clean(leadingYear[2]);
      }
      const pattern = /([A-Za-z][A-Za-z-]*(?:\s+[A-Za-z][A-Za-z-]*){0,2})\s+(\d{4}[a-z]?)([^,;]*)/gi;
      let match;
      while ((match = pattern.exec(segment)) !== null) {
        const author = normalizeModelAuthor(match[1]);
        if (!author) continue;
        currentAuthor = author;
        addModelToken(tokens, author, match[2], match[3]);
      }
    });
    return Array.from(tokens.values());
  }

  function addModelToken(tokens, author, year, suffix) {
    const cleanAuthor = normalizeModelAuthor(author);
    if (!cleanAuthor || !year) return;
    const rawKey = canonicalModelKey(`${cleanAuthor} ${year}`);
    const key = hildebrandCombinedKey(rawKey);
    const algorithm = /\balgorithm\b/i.test(suffix || "") ? " algorithm" : "";
    const label = key === "hildebrand 2014 2017" ? "Hildebrand 2014/2017 cutpoints" : `${cleanAuthor} ${year}${algorithm}`;
    if (!tokens.has(key)) tokens.set(key, { key, label });
  }

  function normalizeModelAuthor(value) {
    let text = clean(value)
      .replace(/^(and|or|for|possibly|others)\s+/i, "")
      .replace(/\b(and|or|for|using|with|from)$/i, "")
      .replace(/\b(open|source|algorithm|algorithms|cutpoints|thresholds)\b/gi, "")
      .replace(/\s+/g, " ")
      .trim();
    if (!text || /^(and|or|for)$/i.test(text)) return "";
    if (/^van\s+hees$/i.test(text)) return "van Hees";
    return text.split(" ").map((part) => /^[A-Z]{2,}$/.test(part) ? part : part.charAt(0).toUpperCase() + part.slice(1)).join(" ");
  }

  function authorYearKey(value) {
    const match = clean(value).replace(/vanHees/gi, "van Hees").match(/([A-Za-z][A-Za-z-]*(?:\s+[A-Za-z][A-Za-z-]*){0,2})\s+(\d{4}[a-z]?)/i);
    if (!match) return "";
    return hildebrandCombinedKey(canonicalModelKey(`${normalizeModelAuthor(match[1])} ${match[2]}`));
  }

  function canonicalModelKey(value) {
    return clean(value).toLowerCase().replace(/[^a-z0-9]+/g, " ").trim();
  }

  function hildebrandCombinedKey(key) {
    return key === "hildebrand 2014" || key === "hildebrand 2017" ? "hildebrand 2014 2017" : key;
  }

  function modelDomainFamily(domainsForModel) {
    const hasSleep = domainsForModel.some((domain) => /sleep/i.test(domain));
    const hasMovement = domainsForModel.some((domain) => /activity/i.test(domain));
    if (hasSleep && !hasMovement) return "sleep";
    if (hasMovement) return "movement";
    return "other";
  }

  function uniqueByKey(rows, keyFn) {
    const seen = new Set();
    return rows.filter((row) => {
      const key = keyFn(row);
      if (!key || seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }

  function renderLandscape() {
    const studies = R.studyCharacteristics;
    renderComplexityTimeline(studies);
    renderContinentCountries(studies);
    renderDevicePeriodicPanel(studies);
    renderParticipantPanel(studies);
    renderModelTypeLandscape(studies);

    document.getElementById("studyIndexCount").textContent = `${studies.length} rows`;
    renderTable("studyIndex", studies.map((row) => ({
      Study: clean(row.study),
      Year: clean(row.year),
      Country: clean(row.country),
      Age: clean(row.age_group),
      Sample: clean(row.sample_size),
      Health: clean(row.health),
      Devices: clean(row.device_brand),
      Method: clean(row.method_best),
      Outcomes: clean(row.outcomes)
    })), 220);
  }

  function renderComplexityTimeline(studies) {
    const groups = [
      "Threshold / rules / linear",
      "Classical machine learning",
      "Temporal / ensemble / mixed",
      "Deep learning",
      "Not reported"
    ];
    const groupColors = {
      "Threshold / rules / linear": colors.Traditional,
      "Classical machine learning": colors["Activity type"],
      "Temporal / ensemble / mixed": colors["Activity intensity"],
      "Deep learning": colors.Sleep,
      "Not reported": "var(--muted)"
    };
    const years = unique(studies.map((row) => clean(row.year))).sort((a, b) => Number(a) - Number(b));
    const byYear = years.map((year) => {
      const rows = studies.filter((row) => clean(row.year) === year);
      const counts = Object.fromEntries(groups.map((group) => [group, 0]));
      rows.forEach((row) => {
        counts[complexityGroup(row.method_best)] += 1;
      });
      return { year, total: rows.length, counts };
    });
    const maxTotal = Math.max(1, ...byYear.map((row) => row.total));
    const width = 1000;
    const height = 390;
    const left = 58;
    const right = 24;
    const top = 78;
    const bottom = 62;
    const chartW = width - left - right;
    const chartH = height - top - bottom;
    const band = chartW / Math.max(1, years.length);
    const barW = Math.max(10, Math.min(34, band * 0.64));
    const y = (value) => top + chartH - (value / maxTotal) * chartH;
    const yTicks = [0, Math.ceil(maxTotal / 2), maxTotal].filter((value, index, arr) => arr.indexOf(value) === index);
    const legend = groups.map((group, index) => {
      const col = index % 3;
      const row = Math.floor(index / 3);
      const x0 = left + col * 292;
      const y0 = 22 + row * 21;
      return `<g class="timeline-legend">
        <rect x="${x0}" y="${y0 - 10}" width="12" height="12" rx="3" fill="${groupColors[group]}"></rect>
        <text x="${x0 + 18}" y="${y0}">${escapeHtml(group)}</text>
      </g>`;
    }).join("");
    const axes = yTicks.map((tick) => `
      <g class="axis">
        <line x1="${left}" x2="${width - right}" y1="${y(tick)}" y2="${y(tick)}" stroke="var(--line)" />
        <text x="${left - 8}" y="${y(tick) + 4}" text-anchor="end">${tick}</text>
      </g>`).join("");
    const bars = byYear.map((row, index) => {
      const x0 = left + index * band + (band - barW) / 2;
      let yCursor = top + chartH;
      const segments = groups.map((group) => {
        const count = row.counts[group];
        if (!count) return "";
        const h = (count / maxTotal) * chartH;
        yCursor -= h;
        const tooltip = `<strong>${escapeHtml(row.year)}</strong><br>${escapeHtml(group)}: ${fmt(count)} studies<br>Total: ${fmt(row.total)}`;
        return `<rect class="timeline-segment" data-tooltip="${escapeAttr(tooltip)}" x="${x0}" y="${yCursor}" width="${barW}" height="${Math.max(1, h)}" rx="3" fill="${groupColors[group]}" stroke="var(--panel-2)" stroke-width="0.8"></rect>`;
      }).join("");
      const labelX = x0 + barW / 2;
      const label = years.length > 18
        ? `<text class="axis-label" x="${labelX}" y="${height - 36}" text-anchor="end" transform="rotate(-45 ${labelX} ${height - 36})">${escapeHtml(row.year)}</text>`
        : `<text class="axis-label" x="${labelX}" y="${height - 34}" text-anchor="middle">${escapeHtml(row.year)}</text>`;
      const totalLabel = row.total
        ? `<text class="bar-total" x="${labelX}" y="${Math.max(top + 12, y(row.total) - 7)}" text-anchor="middle">${fmt(row.total)}</text>`
        : "";
      return `<g>${segments}${totalLabel}${label}</g>`;
    }).join("");
    const container = document.getElementById("yearChart");
    container.innerHTML = `
      <svg class="chart-svg timeline-svg" viewBox="0 0 ${width} ${height}" height="${height}" role="img" aria-label="Publication timeline by model complexity">
        <rect width="${width}" height="${height}" fill="var(--panel-2)"></rect>
        ${legend}
        ${axes}
        <line x1="${left}" x2="${width - right}" y1="${top + chartH}" y2="${top + chartH}" stroke="var(--line-strong)" />
        <line x1="${left}" x2="${left}" y1="${top}" y2="${top + chartH}" stroke="var(--line-strong)" />
        ${bars}
        <text class="axis-title" x="${left + chartW / 2}" y="${height - 6}" text-anchor="middle">Publication year</text>
        <text class="axis-title" x="18" y="${top + chartH / 2}" text-anchor="middle" transform="rotate(-90 18 ${top + chartH / 2})">Studies</text>
      </svg>`;
    bindTooltips(container);
  }

  function renderContinentCountries(studies) {
    const countryEntries = flatten(studies.map((row) => tokenize(row.country)));
    const countryCounts = countValues(countryEntries).map((row, index) => ({
      ...row,
      continent: continentForCountry(row.name),
      color: categoricalColor(index)
    }));
    const grouped = Object.entries(groupBy(countryCounts, (row) => row.continent))
      .map(([continent, countries]) => ({
        continent,
        countries: countries.sort((a, b) => b.count - a.count || a.name.localeCompare(b.name)),
        total: countries.reduce((sum, row) => sum + row.count, 0)
      }))
      .sort((a, b) => b.total - a.total || a.continent.localeCompare(b.continent));
    const width = 1000;
    const height = 390;
    const left = 142;
    const right = 58;
    const top = 46;
    const bottom = 58;
    const chartW = width - left - right;
    const chartH = height - top - bottom;
    const maxTotal = Math.max(1, ...grouped.map((row) => row.total));
    const band = chartH / Math.max(1, grouped.length);
    const barH = Math.max(20, Math.min(38, band * 0.56));
    const x = (value) => left + (value / maxTotal) * chartW;
    const ticks = [0, Math.ceil(maxTotal / 2), maxTotal].filter((value, index, arr) => arr.indexOf(value) === index);
    const axes = ticks.map((tick) => `
      <g class="axis">
        <line x1="${x(tick)}" x2="${x(tick)}" y1="${top - 8}" y2="${top + chartH}" stroke="var(--line)" />
        <text x="${x(tick)}" y="${height - 22}" text-anchor="middle">${tick}</text>
      </g>`).join("");
    const rows = grouped.map((row, rowIndex) => {
      const y0 = top + rowIndex * band + (band - barH) / 2;
      let xCursor = left;
      const segments = row.countries.map((country) => {
        const w = Math.max(2, (country.count / maxTotal) * chartW);
        const tooltip = `<strong>${escapeHtml(country.name)}</strong><br>${escapeHtml(row.continent)}<br>${fmt(country.count)} study entries`;
        const rect = `<rect class="country-segment" data-tooltip="${escapeAttr(tooltip)}" x="${xCursor}" y="${y0}" width="${w}" height="${barH}" rx="3" fill="${country.color}" stroke="var(--panel-2)" stroke-width="1"></rect>`;
        xCursor += w;
        return rect;
      }).join("");
      const yText = y0 + barH / 2 + 4;
      return `<g>
        <text class="country-label" x="${left - 10}" y="${yText}" text-anchor="end">${escapeHtml(row.continent)}</text>
        ${segments}
        <text class="country-count" x="${x(row.total) + 8}" y="${yText}">${fmt(row.total)}</text>
      </g>`;
    }).join("");
    const container = document.getElementById("countryChart");
    container.innerHTML = `
      <svg class="chart-svg country-svg" viewBox="0 0 ${width} ${height}" height="${height}" role="img" aria-label="Study entries by continent and country">
        <rect width="${width}" height="${height}" fill="var(--panel-2)"></rect>
        ${axes}
        <line x1="${left}" x2="${width - right}" y1="${top + chartH}" y2="${top + chartH}" stroke="var(--line-strong)" />
        ${rows}
        <text class="axis-title" x="${left + chartW / 2}" y="${height - 6}" text-anchor="middle">Study entries</text>
      </svg>`;
    bindTooltips(container);
  }

  function renderDevicePeriodicPanel(studies) {
    const rows = countValues(flatten(studies.map((row) => tokenize(row.device_brand))))
      .filter((row) => !isNotReported(row.name));
    const totalMentions = rows.reduce((sum, row) => sum + row.count, 0);
    const multiBrandRows = studies.filter((row) => tokenize(row.device_brand).filter((brand) => !isNotReported(brand)).length > 1).length;
    const maxMentions = Math.max(1, ...rows.map((row) => row.count));
    const top = rows.slice(0, 18);
    const tiles = top.map((entry, index) => {
      const symbol = brandSymbol(entry.name, index);
      const share = totalMentions ? entry.count / totalMentions : NaN;
      const heat = heatColor(entry.count, maxMentions);
      const tooltip = `<strong>${escapeHtml(entry.name)}</strong><br>${fmt(entry.count)} study-characteristics brand mentions<br>${pct(share, 1)} of brand mentions`;
      return `<div class="device-tile" data-tooltip="${escapeAttr(tooltip)}" style="--tile-accent:${heat};--tile-ink:${heatInk(entry.count, maxMentions)}">
        <span class="device-symbol">${escapeHtml(symbol)}</span>
        <strong>${escapeHtml(entry.name)}</strong>
        <small>${fmt(entry.count)} rows | ${pct(share, 1)}</small>
      </div>`;
    }).join("");

    const container = document.getElementById("deviceOutcomeChart");
    container.innerHTML = `
      <div class="metric-grid compact">
        ${metricCardMarkup("Device brands", fmt(rows.length), "Unique brands in Study_Characteristics.csv")}
        ${metricCardMarkup("Most frequent brand", top[0] ? top[0].name : "n.a.", top[0] ? `${fmt(top[0].count)} study-characteristics mentions` : "", "small-value")}
        ${metricCardMarkup("Multi-brand study rows", fmt(multiBrandRows), "Study rows listing more than one brand")}
      </div>
      <div class="heat-legend"><span>Fewer mentions</span><span class="heat-ramp"></span><span>More mentions</span></div>
      <div class="device-periodic-grid">${tiles}</div>`;
    bindTooltips(container);
  }

  function renderParticipantPanel(studies) {
    const sampleValues = studies.map((row) => toNumber(row.sample_size)).filter(Number.isFinite);
    const pairedFemale = studies.map((row) => ({
      sample: toNumber(row.sample_size),
      female: toNumber(row.females)
    })).filter((row) => Number.isFinite(row.sample) && Number.isFinite(row.female));
    const totalPairedSample = pairedFemale.reduce((sum, row) => sum + row.sample, 0);
    const totalFemale = pairedFemale.reduce((sum, row) => sum + row.female, 0);
    const femalePct = totalPairedSample ? totalFemale / totalPairedSample : NaN;
    const ethnicityCounts = countValues(studies.map((row) => clean(row.ethnicity_reported) || "n.r."));
    const ethnicityReported = studies.filter((row) => {
      const value = clean(row.ethnicity_reported).toLowerCase();
      return value && !/^n\.?r\.?$|^no$|^not reported$/.test(value);
    }).length;
    const healthCountsAll = countValues(flatten(studies.map((row) => tokenize(row.health))));
    const healthCounts = healthCountsAll.slice(0, 8);
    const ageCountsAll = countValues(flatten(studies.map((row) => tokenize(row.age_group))));
    const ageCounts = ageCountsAll.slice(0, 8);
    const healthTotal = healthCounts.reduce((sum, row) => sum + row.count, 0);
    const ageTotal = ageCounts.reduce((sum, row) => sum + row.count, 0);

    document.getElementById("sampleChart").innerHTML = `
      <div class="metric-grid compact">
        ${metricCardMarkup("Median sample size", fmt(median(sampleValues)), iqrText(sampleValues))}
        ${metricCardMarkup("Largest sample", fmt(max(sampleValues)), "Study-characteristics extraction")}
        ${metricCardMarkup("Female proportion", pct(femalePct, 1), `${fmt(pairedFemale.length)} rows with sample size and female count`)}
        ${metricCardMarkup("Ethnicity reported", pct(ethnicityReported / Math.max(1, studies.length), 1), `${fmt(ethnicityReported)} of ${fmt(studies.length)} study rows`)}
      </div>
      <div class="split-layout nested-split participant-pie-grid">
        <div>
          <h3 class="mini-title">Health status</h3>
          ${pieBlockMarkup(healthCounts, healthTotal, "Health status")}
        </div>
        <div>
          <h3 class="mini-title">Age groups</h3>
          ${pieBlockMarkup(ageCounts, ageTotal, "Age groups")}
        </div>
      </div>
      <h3 class="mini-title">Ethnicity reporting</h3>
      <p class="compact-note">Ethnicity reporting follows the extraction coding: 1 means ethnicity was reported.</p>
      ${proportionListMarkup(ethnicityCounts, studies.length, "study rows")}`;
    bindTooltips(document.getElementById("sampleChart"));
  }

  function renderModelTypeLandscape(studies) {
    const grouped = new Map();
    studies.forEach((row) => {
      const method = clean(row.method_best) || "Not reported";
      const subMethod = clean(row.sub_method_best) || "Not reported";
      const key = `${method}||${subMethod}`;
      if (!grouped.has(key)) {
        grouped.set(key, {
          method,
          subMethod,
          label: modelSubtypeLabel(subMethod),
          symbol: modelSymbol(subMethod, method),
          count: 0,
          outcomes: new Set()
        });
      }
      const entry = grouped.get(key);
      entry.count += 1;
      tokenize(row.outcomes).forEach((item) => entry.outcomes.add(item));
    });

    const models = Array.from(grouped.values())
      .map((entry) => ({
        ...entry,
        outcomes: Array.from(entry.outcomes).sort(outcomeSort),
        complexity: methodComplexityRank(entry.method)
      }))
      .sort((a, b) => a.complexity - b.complexity || b.count - a.count || a.label.localeCompare(b.label));

    const familyEntries = Object.entries(groupBy(models, (model) => model.method)).map(([method, familyModels]) => ({
      method,
      models: familyModels.sort((a, b) => b.count - a.count || a.label.localeCompare(b.label)),
      count: familyModels.reduce((sum, model) => sum + model.count, 0),
      complexity: methodComplexityRank(method)
    })).sort((a, b) => a.complexity - b.complexity || a.method.localeCompare(b.method));

    const maxCount = Math.max(1, ...models.map((row) => row.count));
    const totalEntries = studies.length;
    const mostFrequent = models.slice().sort((a, b) => b.count - a.count || a.label.localeCompare(b.label))[0];
    const familySections = familyEntries.map((family) => {
      const familyColor = modelFamilyColor(family.method);
      return `
        <section class="model-family-block" style="--family-color:${familyColor}">
          <div class="model-family-head">
            <strong>${escapeHtml(family.method)}</strong>
            <span>${fmt(family.count)} rows</span>
          </div>
          <div class="model-periodic-mini-grid">
            ${family.models.map((model) => modelTileMarkup(model, maxCount, totalEntries)).join("")}
          </div>
        </section>`;
    }).join("");

    document.getElementById("modelTypeLandscape").innerHTML = `
      <div class="metric-grid compact">
        ${metricCardMarkup("Model entries", fmt(totalEntries), "One best-performing prediction model type per study row")}
        ${metricCardMarkup("Model sub-types", fmt(models.length), "Distinct values in method_best and sub_method_best")}
        ${metricCardMarkup("Model families", fmt(familyEntries.length), "Distinct values in method_best")}
        ${metricCardMarkup("Most frequent", mostFrequent ? mostFrequent.label : "n.a.", mostFrequent ? `${fmt(mostFrequent.count)} study rows` : "", "small-value")}
      </div>
      <p class="compact-note">This chart groups prediction model sub-types by modelling family. Tile colour intensity reflects the number of study-characteristics rows; hover for method, outcome, and row-count details.</p>
      <div class="model-periodic-toolbar">
        <div class="heat-legend"><span>Fewer rows</span><span class="heat-ramp"></span><span>More rows</span></div>
      </div>
      <div class="model-periodic-wrap model-periodic-compact">
        <div class="model-family-grid">
          ${familySections}
        </div>
      </div>
      <div class="model-abbreviation-legend compact">
        <div class="model-abbreviation-head">
          <strong>Model abbreviations</strong>
          <span>Tile symbols used in the periodic chart</span>
        </div>
        <div class="model-abbreviation-grid">
          ${modelAbbreviationLegendMarkup(models)}
        </div>
      </div>`;
    bindTooltips(document.getElementById("modelTypeLandscape"));
  }

  function modelTileMarkup(model, maxCount, totalEntries) {
    const heat = heatColor(model.count, maxCount);
    const share = model.count / Math.max(1, totalEntries);
    const outcomes = model.outcomes.length ? model.outcomes : ["Not reported"];
    const tooltip = `<strong>${escapeHtml(model.label)}</strong><br>${escapeHtml(model.method)}<br>${fmt(model.count)} study rows (${pct(share, 1)})<br>Outcomes: ${escapeHtml(outcomes.join(", "))}`;
    const label = `${model.symbol}: ${model.label}, ${fmt(model.count)} study rows`;
    return `<article class="model-periodic-tile compact-model-tile" data-tooltip="${escapeAttr(tooltip)}" aria-label="${escapeAttr(label)}" style="--tile-accent:${heat};--tile-ink:${heatInk(model.count, maxCount)}">
      <span class="model-symbol">${escapeHtml(model.symbol)}</span>
    </article>`;
  }

  function modelSubtypeLabel(value) {
    const text = clean(value) || "Not reported";
    const map = {
      "ANN": "ANN",
      "ANN + DT": "ANN + decision tree",
      "CNN": "CNN",
      "CNN_Attention": "CNN attention",
      "CNN_Fusion": "CNN fusion",
      "CNN_HMM": "CNN-HMM",
      "CNN_LSTM": "CNN-LSTM",
      "DecisionTree": "Decision tree",
      "GradientBoosting": "Gradient boosting",
      "GRU_RNN": "GRU-RNN",
      "HeuristicRule": "Heuristic rule",
      "HMM": "HMM",
      "LinearRegression": "Linear regression",
      "LogisticRegression": "Logistic regression",
      "MixedEffects": "Mixed effects",
      "NaiveBayes": "Naive Bayes",
      "Polynomial": "Polynomial",
      "QuadraticDiscriminant": "Quadratic discriminant",
      "RandomForest": "Random forest",
      "RF + HMM": "RF-HMM",
      "StateSpace": "State space",
      "SVM": "SVM",
      "Threshold": "Threshold"
    };
    return map[text] || text.replace(/_/g, " ");
  }

  function modelSymbol(subMethod, method) {
    const text = clean(subMethod) || clean(method) || "Model";
    const map = {
      "ANN": "ANN",
      "ANN + DT": "AD",
      "CNN": "CNN",
      "CNN_Attention": "CA",
      "CNN_Fusion": "CF",
      "CNN_HMM": "CH",
      "CNN_LSTM": "CL",
      "DecisionTree": "DT",
      "GradientBoosting": "GB",
      "GRU_RNN": "GRU",
      "HeuristicRule": "HR",
      "HMM": "HMM",
      "LinearRegression": "LR",
      "LogisticRegression": "LogR",
      "MixedEffects": "ME",
      "NaiveBayes": "NB",
      "Polynomial": "Poly",
      "QuadraticDiscriminant": "QD",
      "RandomForest": "RF",
      "RF + HMM": "RH",
      "StateSpace": "SS",
      "SVM": "SVM",
      "Threshold": "Th"
    };
    if (map[text]) return map[text];
    const words = modelSubtypeLabel(text).replace(/[^A-Za-z0-9]+/g, " ").split(" ").filter(Boolean);
    if (!words.length) return "M";
    if (words.length === 1) return words[0].slice(0, 3).toUpperCase();
    return words.map((word) => word[0]).join("").slice(0, 3).toUpperCase();
  }

  function modelFamilyColor(method) {
    const text = clean(method).toLowerCase();
    if (/threshold|cutpoint/.test(text)) return colors.Traditional;
    if (/heuristic|rule/.test(text)) return "#2F8F9D";
    if (/linear|glm/.test(text)) return colors["Activity intensity"];
    if (/non-linear|mixed/.test(text)) return "#8A63D2";
    if (/classical|machine|ml/.test(text)) return colors["Non-traditional"];
    if (/probabilistic|temporal/.test(text)) return colors["Activity type"];
    if (/hybrid|ensemble/.test(text)) return "#7A7F2B";
    if (/deep/.test(text)) return colors.Sleep;
    return "var(--muted)";
  }

  function modelAbbreviationLegendMarkup(models) {
    const entries = uniqueByKey(models.slice().sort((a, b) => a.symbol.localeCompare(b.symbol) || a.label.localeCompare(b.label)), (model) => model.symbol)
      .map((model) => ({
        symbol: model.symbol,
        label: modelAbbreviationDefinition(model.subMethod, model.method, model.label)
      }));
    return entries.map((entry) => `
      <span><strong>${escapeHtml(entry.symbol)}</strong>${escapeHtml(entry.label)}</span>`).join("");
  }

  function modelAbbreviationDefinition(subMethod, method, fallbackLabel) {
    const text = clean(subMethod) || clean(method);
    const map = {
      "ANN": "Artificial neural network",
      "ANN + DT": "Artificial neural network plus decision tree",
      "CNN": "Convolutional neural network",
      "CNN_Attention": "Convolutional neural network with attention",
      "CNN_Fusion": "Convolutional neural network fusion model",
      "CNN_HMM": "Convolutional neural network plus hidden Markov model",
      "CNN_LSTM": "Convolutional neural network plus long short-term memory",
      "DecisionTree": "Decision tree",
      "GradientBoosting": "Gradient boosting",
      "GRU_RNN": "Gated recurrent unit recurrent neural network",
      "HeuristicRule": "Heuristic rule",
      "HMM": "Hidden Markov model",
      "LinearRegression": "Linear regression",
      "LogisticRegression": "Logistic regression",
      "MixedEffects": "Mixed-effects model",
      "NaiveBayes": "Naive Bayes",
      "Polynomial": "Polynomial model",
      "QuadraticDiscriminant": "Quadratic discriminant analysis",
      "RandomForest": "Random forest",
      "RF + HMM": "Random forest plus hidden Markov model",
      "StateSpace": "State-space model",
      "SVM": "Support vector machine",
      "Threshold": "Threshold or cutpoint"
    };
    return map[text] || fallbackLabel || text || "Model subtype";
  }

  function outcomeSort(a, b) {
    const order = ["Activity intensity", "Activity type", "Sleep-Wake", "Sleep", "Energy expenditure"];
    const ai = order.findIndex((item) => clean(a).toLowerCase() === item.toLowerCase());
    const bi = order.findIndex((item) => clean(b).toLowerCase() === item.toLowerCase());
    return (ai === -1 ? 99 : ai) - (bi === -1 ? 99 : bi) || clean(a).localeCompare(clean(b));
  }

  function complexityGroup(method) {
    const text = clean(method).toLowerCase();
    if (!text || text === "n.r.") return "Not reported";
    if (/threshold|cutpoint|heuristic|linear|glm|rule/.test(text)) return "Threshold / rules / linear";
    if (/classical ml|random|svm|boost|forest|tree|machine/.test(text)) return "Classical machine learning";
    if (/probabilistic|temporal|hybrid|ensemble|mixed|non-linear/.test(text)) return "Temporal / ensemble / mixed";
    if (/deep/.test(text)) return "Deep learning";
    return "Temporal / ensemble / mixed";
  }

  function methodComplexityRank(method) {
    const text = clean(method).toLowerCase();
    if (/threshold|cutpoint/.test(text)) return 1;
    if (/heuristic|rule/.test(text)) return 2;
    if (/linear|glm/.test(text)) return 3;
    if (/non-linear|mixed/.test(text)) return 4;
    if (/classical|machine|ml/.test(text)) return 5;
    if (/probabilistic|temporal/.test(text)) return 6;
    if (/hybrid|ensemble/.test(text)) return 7;
    if (/deep/.test(text)) return 8;
    return 9;
  }

  function topIntersections(rows, leftColumn, rightColumn, limit) {
    const counts = new Map();
    rows.forEach((row) => {
      const left = tokenize(row[leftColumn]);
      const right = tokenize(row[rightColumn]);
      left.forEach((a) => right.forEach((b) => {
        const key = `${a} + ${b}`;
        counts.set(key, (counts.get(key) || 0) + 1);
      }));
    });
    return Array.from(counts, ([name, count]) => ({ name, count }))
      .sort((a, b) => b.count - a.count || a.name.localeCompare(b.name))
      .slice(0, limit);
  }

  function continentForCountry(country) {
    const c = clean(country);
    const map = {
      "Australia": "Oceania",
      "Brazil": "South America",
      "Norway": "Europe",
      "United Kingdom": "Europe",
      "United States of America": "North America",
      "Spain": "Europe",
      "Netherlands": "Europe",
      "Japan": "Asia",
      "Denmark": "Europe",
      "Canada": "North America",
      "China": "Asia",
      "Taiwan": "Asia",
      "South Korea": "Asia",
      "Switzerland": "Europe",
      "Germany": "Europe",
      "France": "Europe",
      "Italy": "Europe",
      "Belgium": "Europe",
      "Czech Republic": "Europe",
      "Estonia": "Europe",
      "Lithuania": "Europe",
      "Sweden": "Europe",
      "Finland": "Europe",
      "Ireland": "Europe",
      "New Zealand": "Oceania",
      "Singapore": "Asia",
      "India": "Asia",
      "Thailand": "Asia",
      "Israel": "Asia",
      "South Africa": "Africa",
      "Chile": "South America",
      "Peru": "South America",
      "Mexico": "North America",
      "Portugal": "Europe",
      "Austria": "Europe"
    };
    return map[c] || "Other";
  }

  function shortQualityDomain(domain) {
    const text = clean(domain).replace(/^Domain\s+\d+:\s*/i, "");
    if (/patient/i.test(text)) return "Patient selection/study design";
    if (/index/i.test(text)) return "Index measure";
    if (/criterion/i.test(text)) return "Criterion measure";
    if (/flow/i.test(text)) return "Flow and timing";
    return text || "Quality domain";
  }

  function countryCoordinate(country) {
    const c = clean(country);
    const map = {
      "Australia": { lat: -25.3, lon: 133.8 },
      "Austria": { lat: 47.5, lon: 14.6 },
      "Belgium": { lat: 50.5, lon: 4.5 },
      "Brazil": { lat: -14.2, lon: -51.9 },
      "Canada": { lat: 56.1, lon: -106.3 },
      "Chile": { lat: -35.7, lon: -71.5 },
      "China": { lat: 35.9, lon: 104.2 },
      "Denmark": { lat: 56.3, lon: 9.5 },
      "Finland": { lat: 61.9, lon: 25.7 },
      "France": { lat: 46.2, lon: 2.2 },
      "Germany": { lat: 51.2, lon: 10.5 },
      "India": { lat: 20.6, lon: 78.9 },
      "Ireland": { lat: 53.4, lon: -8.2 },
      "Israel": { lat: 31.0, lon: 34.9 },
      "Italy": { lat: 41.9, lon: 12.6 },
      "Japan": { lat: 36.2, lon: 138.3 },
      "Mexico": { lat: 23.6, lon: -102.5 },
      "Netherlands": { lat: 52.1, lon: 5.3 },
      "New Zealand": { lat: -40.9, lon: 174.9 },
      "Norway": { lat: 60.5, lon: 8.5 },
      "Portugal": { lat: 39.4, lon: -8.2 },
      "Singapore": { lat: 1.4, lon: 103.8 },
      "South Africa": { lat: -30.6, lon: 22.9 },
      "South Korea": { lat: 36.5, lon: 127.8 },
      "Spain": { lat: 40.5, lon: -3.7 },
      "Sweden": { lat: 60.1, lon: 18.6 },
      "Switzerland": { lat: 46.8, lon: 8.2 },
      "Thailand": { lat: 15.9, lon: 100.9 },
      "United Kingdom": { lat: 55.4, lon: -3.4 },
      "United States of America": { lat: 39.8, lon: -98.6 }
    };
    return map[c] || { lat: 0, lon: 0 };
  }

  function countryShortLabel(country) {
    const map = {
      "United States of America": "USA",
      "United Kingdom": "UK",
      "South Korea": "Korea",
      "South Africa": "S. Africa",
      "New Zealand": "NZ"
    };
    return map[clean(country)] || clean(country);
  }

  function brandSymbol(brand, index) {
    const letters = clean(brand).replace(/[^A-Za-z0-9]/g, "").toUpperCase();
    if (!letters) return String(index + 1);
    if (letters.length === 1) return letters;
    return letters.slice(0, 2);
  }

  function deviceTileColor(index) {
    const palette = [colors.Traditional, colors["Non-traditional"], colors["Activity intensity"], colors.Sleep, colors["Activity type"], colors.High];
    return palette[index % palette.length];
  }

  function categoricalColor(index) {
    const palette = [
      "#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9",
      "#D55E00", "#8A63D2", "#7A7F2B", "#2F8F9D", "#B65C82",
      "#5A8F3C", "#B7791F"
    ];
    return palette[index % palette.length];
  }

  function heatColor(value, maxValue) {
    const t = Math.max(0, Math.min(1, value / Math.max(1, maxValue)));
    if (t < 0.5) return blendHex("#56B4E9", "#F1B844", t * 2);
    return blendHex("#F1B844", "#D55E00", (t - 0.5) * 2);
  }

  function heatInk(value, maxValue) {
    return value / Math.max(1, maxValue) < 0.56 ? "#111418" : "#ffffff";
  }

  function blendHex(a, b, t) {
    const ca = hexToRgb(a);
    const cb = hexToRgb(b);
    return rgbToHex(
      Math.round(ca.r + (cb.r - ca.r) * t),
      Math.round(ca.g + (cb.g - ca.g) * t),
      Math.round(ca.b + (cb.b - ca.b) * t)
    );
  }

  function hexToRgb(hex) {
    const value = hex.replace("#", "");
    return {
      r: parseInt(value.slice(0, 2), 16),
      g: parseInt(value.slice(2, 4), 16),
      b: parseInt(value.slice(4, 6), 16)
    };
  }

  function rgbToHex(r, g, b) {
    return "#" + [r, g, b].map((value) => value.toString(16).padStart(2, "0")).join("");
  }

  function renderQualityToolGuide() {
    const target = document.getElementById("qualityGuideDetails");
    if (!target) return;
    const tool = DATA.qualityAssessmentTool || {};
    const questions = Array.isArray(tool.questions) ? tool.questions : [];
    if (!questions.length) {
      target.innerHTML = `<div><strong>Guide unavailable</strong><p>The quality assessment tool file was not found in the app data bundle.</p></div>`;
      return;
    }

    const grouped = groupBy(questions, (question) => clean(question.domain) || "Quality domain");
    target.innerHTML = Object.entries(grouped).map(([domain, domainQuestions]) => {
      const code = domain.match(/Domain\s+(\d+)/i);
      const domainCode = code ? `D${code[1]}` : "Domain";
      const questionMarkup = domainQuestions.map((question) => `
        <div class="quality-question">
          <strong>${escapeHtml(question.item)}</strong>
          <p>${escapeHtml(signallingQuestionText(question))}</p>
        </div>`).join("");
      return `<div class="quality-guide-domain">
        <strong>${escapeHtml(domainCode)}. ${escapeHtml(shortQualityDomain(domain))}</strong>
        <div class="quality-question-list">${questionMarkup}</div>
      </div>`;
    }).join("");
  }

  function renderQuality() {
    const allRows = R.qualityAssessment;
    const rows = filteredQualityRows();
    const anyHigh = rows.filter(hasHighQualityRisk).length;
    const allLow = rows.filter((row) => qualityDomainDefs.every((domain) => riskLabel(row[domain.key]) === "Low")).length;
    const signalTotals = rows.reduce((totals, row) => {
      const counts = signallingCounts(row);
      totals.yes += counts.Yes;
      totals.no += counts.No;
      totals.unclear += counts.Unclear;
      totals.na += counts.NA;
      return totals;
    }, { yes: 0, no: 0, unclear: 0, na: 0 });

    metricCards("qualitySummaryCards", [
      ["Studies assessed", fmt(allRows.length), "Rows in Quality_Assessment.csv"],
      ["Filtered studies", fmt(rows.length), state.qualityRisk === "ALL" ? "All quality rows shown" : "Risk filter active"],
      ["Any high-risk domain", fmt(anyHigh), "At least one QUADAS-2 domain rated high"],
      ["All domains low risk", fmt(allLow), "Low risk across all four domains"],
      ["SQ No / Unclear / NA", `${fmt(signalTotals.no)} / ${fmt(signalTotals.unclear)} / ${fmt(signalTotals.na)}`, "Signalling-question responses in current view"]
    ]);

    renderQualityDomainRisk(rows);
    renderQualitySignalSummary(rows);
    renderQualityStudyCards(rows);
    renderQualityStudyTable(rows);
  }

  function renderQualityDomainRisk(rows) {
    if (!rows.length) {
      document.getElementById("qualityBars").innerHTML = `<div class="empty-state">No quality rows match the current filters.</div>`;
      return;
    }
    const summary = qualityDomainDefs.flatMap((domainDef) => {
      const counts = countValues(rows.map((row) => riskLabel(row[domainDef.key])));
      const total = counts.reduce((sum, row) => sum + row.count, 0);
      return counts.map((row) => ({
        Domain: domainDef.short,
        Rating: row.name,
        n: row.count,
        Percent: total ? 100 * row.count / total : 0
      }));
    });
    const domainsByName = groupBy(summary, (row) => row.Domain);
    const html = Object.entries(domainsByName).map(([domain, rows]) => {
      const low = rows.find((row) => row.Rating === "Low");
      const high = rows.find((row) => row.Rating === "High");
      const unclear = rows.find((row) => row.Rating === "Unclear");
      const lowPct = low ? toNumber(low.Percent) : 0;
      const highPct = high ? toNumber(high.Percent) : 0;
      const unclearPct = unclear ? toNumber(unclear.Percent) : 0;
      return `<div class="info-item">
        <strong>${escapeHtml(domain)}</strong>
        <div class="stacked-risk" aria-label="${escapeAttr(domain)} risk summary">
          ${lowPct ? `<div class="risk-piece risk-low" style="width:${lowPct}%">${lowPct.toFixed(0)}%</div>` : ""}
          ${highPct ? `<div class="risk-piece risk-high" style="width:${highPct}%">${highPct.toFixed(0)}%</div>` : ""}
          ${unclearPct ? `<div class="risk-piece risk-unclear" style="width:${unclearPct}%">${unclearPct.toFixed(0)}%</div>` : ""}
        </div>
        <p>Low risk N=${low ? escapeHtml(low.n) : "0"}; high risk N=${high ? escapeHtml(high.n) : "0"}; unclear N=${unclear ? escapeHtml(unclear.n) : "0"}.</p>
      </div>`;
    }).join("");
    document.getElementById("qualityBars").innerHTML = `<div class="info-list">${html}</div>`;
  }

  function renderQualitySignalSummary(rows) {
    if (!rows.length) {
      document.getElementById("qualitySignals").innerHTML = `<div class="empty-state">No signalling-question rows match the current filters.</div>`;
      return;
    }
    const html = signallingQuestions.map((question) => {
      const counts = {
        Yes: rows.filter((row) => signalValue(row[question.key]) === "Yes").length,
        No: rows.filter((row) => signalValue(row[question.key]) === "No").length,
        Unclear: rows.filter((row) => signalValue(row[question.key]) === "Unclear").length,
        NA: rows.filter((row) => signalValue(row[question.key]) === "NA").length
      };
      const total = Math.max(1, rows.length);
      const yesPct = counts.Yes / total * 100;
      const noPct = counts.No / total * 100;
      const unclearPct = counts.Unclear / total * 100;
      const naPct = counts.NA / total * 100;
      const tooltip = signallingQuestionTooltip(question);
      return `<div class="signal-row">
        <div class="signal-label">
          <strong>${escapeHtml(question.label)}</strong>
          <span>${escapeHtml(question.domain)}</span>
        </div>
        <div class="signal-track" aria-label="${escapeAttr(question.label)} response summary" ${tooltip ? `data-tooltip="${escapeAttr(tooltip)}"` : ""}>
          <div class="signal-piece signal-yes" style="width:${yesPct}%">Y ${counts.Yes}</div>
          <div class="signal-piece signal-no" style="width:${noPct}%">N ${counts.No}</div>
          <div class="signal-piece signal-unclear" style="width:${unclearPct}%">U ${counts.Unclear}</div>
          <div class="signal-piece signal-na" style="width:${naPct}%">NA ${counts.NA}</div>
        </div>
      </div>`;
    }).join("");
    document.getElementById("qualitySignals").innerHTML = `<div class="signal-list">${html}</div>`;
    bindTooltips(document.getElementById("qualitySignals"));
  }

  function renderQualityStudyCards(rows) {
    const countNode = document.getElementById("qualityStudyCount");
    countNode.textContent = `${fmt(rows.length)} studies`;
    const shown = rows.slice(0, 120);
    if (!shown.length) {
      document.getElementById("qualityStudyCards").innerHTML = `<div class="empty-state">No studies match the current quality filters.</div>`;
      return;
    }
    const more = rows.length > shown.length ? `<p class="metric-sub">Showing ${fmt(shown.length)} of ${fmt(rows.length)} studies. Refine the search to inspect hidden rows.</p>` : "";
    const html = shown.map((row) => {
      const highCount = qualityDomainDefs.filter((domain) => riskLabel(row[domain.key]) === "High").length;
      const unclearCount = qualityDomainDefs.filter((domain) => riskLabel(row[domain.key]) === "Unclear").length;
      const signal = signallingCounts(row);
      const riskChips = qualityDomainDefs.map((domain) => {
        const rating = riskLabel(row[domain.key]);
        return `<span class="risk-chip ${responseClass(rating)}">${escapeHtml(domain.code)} ${escapeHtml(rating)}</span>`;
      }).join("");
      const signalChips = signallingQuestions.map((question) => {
        const value = signalValue(row[question.key]);
        return `<span class="signal-chip ${responseClass(value)}" data-tooltip="${escapeAttr(signallingQuestionTooltip(question))}">${escapeHtml(question.key)} ${escapeHtml(value)}</span>`;
      }).join("");
      return `<details class="quality-card">
        <summary>
          <span>
            <strong>${escapeHtml(clean(row["Study ID"]) || "Study n.r.")}</strong>
            <em>${escapeHtml(truncate(row.Title, 110))}</em>
          </span>
          <span class="quality-card-score">${highCount ? `${highCount}/4 high` : (unclearCount ? `${unclearCount}/4 unclear` : "All low")}</span>
        </summary>
        <div class="quality-card-body">
          <div class="risk-chip-row">${riskChips}</div>
          <p class="metric-sub">Signalling responses: ${fmt(signal.Yes)} Yes, ${fmt(signal.No)} No, ${fmt(signal.Unclear)} Unclear, ${fmt(signal.NA)} NA.</p>
          <div class="signal-chip-grid">${signalChips}</div>
        </div>
      </details>`;
    }).join("");
    document.getElementById("qualityStudyCards").innerHTML = more + html;
    bindTooltips(document.getElementById("qualityStudyCards"));
  }

  function renderQualityStudyTable(rows) {
    const tableRows = rows.map((row) => {
      const signal = signallingCounts(row);
      const base = {
        "Study ID": clean(row["Study ID"]),
        Title: clean(row.Title),
        "High-risk domains": qualityDomainDefs.filter((domain) => riskLabel(row[domain.key]) === "High").length,
        "SQ Yes": signal.Yes,
        "SQ No": signal.No,
        "SQ Unclear": signal.Unclear,
        "SQ NA": signal.NA
      };
      qualityDomainDefs.forEach((domain) => {
        base[domain.short] = riskLabel(row[domain.key]);
      });
      signallingQuestions.forEach((question) => {
        base[question.key] = signalValue(row[question.key]);
      });
      return base;
    });
    renderTable("qualityStudyTable", tableRows, 220);
  }

  function filteredQualityRows() {
    const search = state.qualitySearch.trim().toLowerCase();
    return R.qualityAssessment.filter((row) => {
      if (search) {
        const haystack = [row["Study ID"], row.Title, row["Covidence #"]].join(" ").toLowerCase();
        if (!haystack.includes(search)) return false;
      }
      if (state.qualityRisk === "ANY_HIGH" && !hasHighQualityRisk(row)) return false;
      if (state.qualityRisk === "ALL_LOW" && !qualityDomainDefs.every((domain) => riskLabel(row[domain.key]) === "Low")) return false;
      if (state.qualityRisk === "D1_HIGH" && riskLabel(row["Domain 1: Patient Selection/Study Design"]) !== "High") return false;
      if (state.qualityRisk === "D2_HIGH" && riskLabel(row["Domain 2: Index Measure"]) !== "High") return false;
      if (state.qualityRisk === "D3_HIGH" && riskLabel(row["Domain 3: Criterion Measure"]) !== "High") return false;
      if (state.qualityRisk === "D4_HIGH" && riskLabel(row["Domain 4: Flow and Timing"]) !== "High") return false;
      return true;
    }).sort((a, b) => clean(a["Study ID"]).localeCompare(clean(b["Study ID"])));
  }

  function hasHighQualityRisk(row) {
    return qualityDomainDefs.some((domain) => riskLabel(row[domain.key]) === "High");
  }

  function signallingCounts(row) {
    return signallingQuestions.reduce((counts, question) => {
      const value = signalValue(row[question.key]);
      counts[value] = (counts[value] || 0) + 1;
      return counts;
    }, { Yes: 0, No: 0, Unclear: 0, NA: 0 });
  }

  function signalValue(value) {
    const text = clean(value).toUpperCase();
    if (text === "YES" || text === "Y") return "Yes";
    if (text === "NO" || text === "N") return "No";
    if (text === "UNCLEAR" || text === "U") return "Unclear";
    return "NA";
  }

  function responseClass(value) {
    if (value === "Yes") return "yes";
    if (value === "No") return "no";
    if (value === "Unclear") return "unclear";
    if (value === "Low") return "low";
    if (value === "High") return "high";
    return "na";
  }

  function renderDocCards(targetId) {
    const target = document.getElementById(targetId);
    if (!target) return;
    const citation = DATA.docs.citation.text;
    const versionMatch = citation.match(/version:\s*([^\n]+)/);
    const titleMatch = citation.match(/title:\s*"([^"]+)"/);
    const cards = [
      ["Author and repository", "Millen J. Theophilus", `<a class="link-button" href="https://github.com/miltheo/SCOPE-MOVE" target="_blank" rel="noopener">Open GitHub repository</a>`],
      ["Software citation", titleMatch ? titleMatch[1] : "SCOPE-MOVE", `<a class="link-button" href="https://github.com/miltheo/SCOPE-MOVE/blob/main/LICENSE" target="_blank" rel="noopener">GPL-3.0-or-later license</a><p>Explorer version ${APP_VERSION}; repository citation metadata version ${versionMatch ? versionMatch[1] : "n.a."}.</p>`],
      ["Associated study", "SCOPE-MOVE is a product of a future scoping review study currently at journal submission stage.", "Use the forthcoming manuscript citation alongside the software release once available."],
      ["Zenodo repository", "Archived software record", `<a class="link-button" href="https://doi.org/10.5281/zenodo.18704633" target="_blank" rel="noopener">Open Zenodo DOI</a><p>Use this for reproducible software citation context.</p>`],
      ["Source rows", "Classifier extraction, study characteristics, quality assessment, and app validation tables are indexed.", `<button class="ghost-button nav-action" data-target-view="data" data-dataset="classifierInput">Open data workbench</button>`]
    ];
    target.innerHTML = cards.map(([title, body, note]) => {
      const noteText = String(note);
      const htmlNote = noteText.startsWith("<a") || noteText.startsWith("<button");
      return `<div class="doc-card"><strong>${escapeHtml(title)}</strong><p>${escapeHtml(String(body))}</p>${htmlNote ? `<div class="doc-note">${noteText}</div>` : `<p>${escapeHtml(noteText)}</p>`}</div>`;
    }).join("");
  }

  function renderStaticDocLinks() {
    const link = document.getElementById("dataDictionaryInstructionLink");
    if (link && DATA.docs.dataDictionary && DATA.docs.dataDictionary.url) {
      link.href = DATA.docs.dataDictionary.url;
    }
  }

  function renderDataWorkbench() {
    const rows = displayRowsForDataset(state.datasetKey);
    const source = DATA.sources[state.datasetKey] || "";
    const query = state.datasetSearch.trim().toLowerCase();
    const filtered = query ? rows.filter((row) => Object.values(row).join(" ").toLowerCase().includes(query)) : rows;
    document.getElementById("datasetTitle").textContent = labels[state.datasetKey] || state.datasetKey;
    document.getElementById("datasetMeta").textContent = `${fmt(filtered.length)} of ${fmt(rows.length)} rows | ${source}`;
    renderTable("datasetTable", filtered, 500);
  }

  function downloadCurrentDataset() {
    const rows = displayRowsForDataset(state.datasetKey);
    const query = state.datasetSearch.trim().toLowerCase();
    const filtered = query ? rows.filter((row) => Object.values(row).join(" ").toLowerCase().includes(query)) : rows;
    const csv = toCsv(filtered);
    const blob = new Blob([csv], { type: "text/csv;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = `${state.datasetKey}.csv`;
    document.body.appendChild(link);
    link.click();
    link.remove();
    URL.revokeObjectURL(url);
  }

  function displayRowsForDataset(key) {
    const rows = R[key] || [];
    const pick = (row, columns) => Object.fromEntries(columns.map(([source, label]) => [label || source, clean(row[source])]));
    if (key === "classifierInput") {
      const columns = [
        ["val_id", "Validation ID"], ["study", "Study"], ["movement_behaviour", "24-hr-MB domain"],
        ["Is the model published previously?", "Model source / prior publication"],
        ["model_type", "Model type"], ["method", "Method"], ["method_sub", "Sub-method"],
        ["model_test_outcomes", "Test outcomes"], ["test_criterion", "Criterion"],
        ["age_group", "Age group"], ["test_device", "Wearable/device"], ["health", "Health"],
        ["test_environment", "Test environment"], ["test_protocol", "Test protocol"], ["validation_type", "Validation type"],
        ["n_participants", "Participants"], ["n_classes", "Classes"], ["k_folds", "CV folds"],
        ["f1_mean", "F1 mean (%)"], ["sens_mean", "Sensitivity mean (%)"], ["spec_mean", "Specificity mean (%)"],
        ["doi", "DOI"]
      ];
      return rows.map((row) => pick(row, columns));
    }
    if (key === "appRemlLong2") {
      return performanceRows.map((row) => ({
        "Validation ID": row.valId,
        Study: row.study,
        Metric: row.metric,
        Estimate: pct(row.estimate, 1),
        "95% CI": `${pct(Math.min(row.ciLow, row.estimate, row.ciHigh), 1)} to ${pct(Math.max(row.ciLow, row.estimate, row.ciHigh), 1)}`,
        Domain: row.domain,
        "Model group": row.methodGroup,
        Method: row.method,
        "Sub-method": row.methodSub,
        "Validation type": row.validation,
        "Validation signal": isExternalValidation(row) ? "External validation" : "",
        "Model source / prior publication": modelSourceText(row),
        Environment: row.environment,
        Protocol: row.protocol,
        "Age group": row.ageGroup,
        Device: row.device,
        Participants: Number.isFinite(row.participants) ? fmt(row.participants) : "",
        DOI: row.doi
      }));
    }
    if (key === "energyInput") {
      const columns = [
        ["val_id", "Validation ID"], ["study", "Study"], ["movement_behaviour", "Domain"],
        ["model_type", "Model type"], ["method", "Method"], ["method_group", "Method group"],
        ["test_criterion", "Criterion"], ["age_group", "Age group"], ["test_device", "Wearable/device"],
        ["test_environment", "Test environment"], ["test_protocol", "Test protocol"], ["validation_type", "Validation type"],
        ["mape_mean", "MAPE mean (%)"], ["mape_sd", "MAPE SD"], ["rmse_met_mean", "RMSE MET mean"],
        ["n_participants", "Participants"], ["k_folds", "CV folds"]
      ];
      return rows.filter((row) => clean(row.id) || clean(row.val_id) || clean(row.study)).map((row) => pick(row, columns));
    }
    if (key === "studyCharacteristics") {
      const columns = [
        ["id", "Study ID"], ["study", "Study"], ["year", "Year"], ["country", "Country"],
        ["age_group", "Age group"], ["sample_size", "Sample size"], ["females", "Females"],
        ["health", "Health"], ["device_brand", "Device brand"], ["sensor_type", "Sensor type"],
        ["acquisition_sampling_rate", "Acquisition sampling rate"], ["sampling_rate_cat", "Sampling-rate category"],
        ["resampled", "Resampled"], ["resampled_sampling_rate", "Resampled sampling rate"],
        ["feature_types", "Feature types"], ["window_size", "Window size"], ["method_best", "Model type"],
        ["sub_method_best", "Sub-method"], ["outcomes", "Outcomes"]
      ];
      return rows.map((row) => pick(row, columns));
    }
    if (key === "qualityAssessment") {
      const columns = [
        ["Study ID", "Study ID"], ["Title", "Title"],
        ["SQ1", "SQ1"], ["SQ2", "SQ2"], ["SQ3", "SQ3"], ["SQ4", "SQ4"],
        ["Domain 1: Patient Selection/Study Design", "Patient selection/study design"],
        ["SQ5", "SQ5"], ["SQ6", "SQ6"],
        ["Domain 2: Index Measure", "Index measure"],
        ["SQ7", "SQ7"], ["SQ8", "SQ8"],
        ["Domain 3: Criterion Measure", "Criterion measure"],
        ["SQ9", "SQ9"], ["SQ10", "SQ10"], ["SQ11", "SQ11"],
        ["Domain 4: Flow and Timing", "Flow and timing"]
      ];
      return rows.map((row) => pick(row, columns));
    }
    return rows.map((row) => {
      const cleaned = {};
      Object.entries(row).forEach(([key, value]) => {
        if (!["Reported?", "CM?", "other", "dynami_range"].includes(key)) cleaned[key] = value;
      });
      return cleaned;
    });
  }

  function metricCards(id, cards) {
    byId(id).innerHTML = cards.map(([label, value, sub, className]) => metricCardMarkup(label, value, sub, className)).join("");
  }

  function metricCardMarkup(label, value, sub, className) {
    const extraClass = className ? ` ${escapeAttr(className)}` : "";
    return `<div class="metric-card${extraClass}">
      <p class="metric-label">${escapeHtml(label)}</p>
      <p class="metric-value">${escapeHtml(String(value))}</p>
      <p class="metric-sub">${escapeHtml(String(sub || ""))}</p>
    </div>`;
  }

  function renderTable(id, rows, limit) {
    const container = document.getElementById(id);
    if (!rows.length) {
      container.innerHTML = `<div class="empty-state">No rows to display.</div>`;
      return;
    }
    const cols = Object.keys(rows[0]);
    const shown = rows.slice(0, limit);
    const more = rows.length > shown.length ? `<p class="metric-sub">Showing ${fmt(shown.length)} of ${fmt(rows.length)} rows.</p>` : "";
    container.innerHTML = `${more}<table><thead><tr>${cols.map((col) => `<th>${escapeHtml(col)}</th>`).join("")}</tr></thead><tbody>${shown.map((row) => (
      `<tr>${cols.map((col) => `<td>${formatCell(row[col])}</td>`).join("")}</tr>`
    )).join("")}</tbody></table>`;
  }

  function renderBarList(id, rows, opts) {
    document.getElementById(id).innerHTML = barListMarkup(rows, opts);
  }

  function barListMarkup(rows, opts) {
    const maxValue = Math.max(1, ...rows.map((row) => row.count));
    const color = opts && opts.color ? opts.color : "#0f7f78";
    const label = opts && opts.valueLabel ? opts.valueLabel : "rows";
    return `<div class="bar-list">${rows.map((row) => {
      const width = Math.max(3, row.count / maxValue * 100);
      const fill = opts && opts.colorByRow && row.color ? row.color : color;
      return `<div class="bar-row">
        <div class="bar-label" title="${escapeAttr(row.name)}">${escapeHtml(row.name)}</div>
        <div class="bar-track"><div class="bar-fill" style="width:${width}%;background:${fill}"></div></div>
        <div class="metric-sub">${fmt(row.count)} ${escapeHtml(label)}</div>
      </div>`;
    }).join("")}</div>`;
  }

  function proportionListMarkup(rows, denominator, label) {
    const total = Math.max(1, denominator || rows.reduce((sum, row) => sum + row.count, 0));
    const maxValue = Math.max(1, ...rows.map((row) => row.count));
    return `<div class="bar-list">${rows.map((row) => {
      const width = Math.max(4, row.count / maxValue * 100);
      const pctText = (row.count / total * 100).toFixed(1);
      return `<div class="bar-row">
        <div class="bar-label" title="${escapeAttr(row.name)}">${escapeHtml(row.name)}</div>
        <div class="bar-track"><div class="bar-fill" style="width:${width}%;background:var(--accent-2)"></div></div>
        <div class="metric-sub">${fmt(row.count)} ${escapeHtml(label || "rows")} | ${pctText}%</div>
      </div>`;
    }).join("")}</div>`;
  }

  function pieBlockMarkup(rows, denominator, label) {
    return `<div class="pie-wrap">
      ${pieChartMarkup(rows, denominator, label)}
      ${pieLegendMarkup(rows, denominator)}
    </div>`;
  }

  function pieLegendMarkup(rows, denominator) {
    const total = Math.max(1, denominator || rows.reduce((sum, row) => sum + row.count, 0));
    return `<div class="pie-legend">${rows.map((row, index) => `
      <div class="pie-legend-row">
        <span class="swatch square" style="background:${categoricalColor(index)}"></span>
        <span title="${escapeAttr(row.name)}">${escapeHtml(row.name)}</span>
        <strong>${fmt(row.count)} (${(row.count / total * 100).toFixed(1)}%)</strong>
      </div>`).join("")}</div>`;
  }

  function pieChartMarkup(rows, denominator, label) {
    const total = Math.max(1, denominator || rows.reduce((sum, row) => sum + row.count, 0));
    const cx = 100;
    const cy = 100;
    const r = 82;
    let start = -Math.PI / 2;
    const slices = rows.map((row, index) => {
      const angle = row.count / total * Math.PI * 2;
      const end = start + angle;
      const color = categoricalColor(index);
      const tooltip = `<strong>${escapeHtml(row.name)}</strong><br>${fmt(row.count)} entries<br>${(row.count / total * 100).toFixed(1)}%`;
      if (angle >= Math.PI * 2 - 0.0001) {
        start = end;
        return `<circle class="pie-slice" data-tooltip="${escapeAttr(tooltip)}" cx="${cx}" cy="${cy}" r="${r}" fill="${color}"></circle>`;
      }
      const p0 = polarPoint(cx, cy, r, start);
      const p1 = polarPoint(cx, cy, r, end);
      const large = angle > Math.PI ? 1 : 0;
      start = end;
      return `<path class="pie-slice" data-tooltip="${escapeAttr(tooltip)}" d="M ${cx} ${cy} L ${p0.x} ${p0.y} A ${r} ${r} 0 ${large} 1 ${p1.x} ${p1.y} Z" fill="${color}" stroke="var(--panel-2)" stroke-width="2"></path>`;
    }).join("");
    return `<svg class="pie-chart" viewBox="0 0 200 200" role="img" aria-label="${escapeAttr(label)} pie chart">
      ${slices}
      <circle cx="${cx}" cy="${cy}" r="42" fill="var(--panel-2)" stroke="var(--line)" stroke-width="1"></circle>
      <text x="${cx}" y="${cy - 2}" text-anchor="middle" fill="var(--ink)" font-size="20" font-weight="900">${fmt(total)}</text>
      <text x="${cx}" y="${cy + 18}" text-anchor="middle" fill="var(--muted)" font-size="11" font-weight="800">entries</text>
    </svg>`;
  }

  function polarPoint(cx, cy, r, angle) {
    return {
      x: cx + r * Math.cos(angle),
      y: cy + r * Math.sin(angle)
    };
  }

  function bindTooltips(container) {
    const tooltip = byId("tooltip");
    container.querySelectorAll("[data-tooltip]").forEach((node) => {
      node.addEventListener("mouseenter", () => {
        tooltip.innerHTML = node.dataset.tooltip;
        tooltip.style.display = "block";
      });
      node.addEventListener("mousemove", (event) => {
        tooltip.style.left = Math.min(window.innerWidth - 380, event.clientX + 16) + "px";
        tooltip.style.top = Math.min(window.innerHeight - 160, event.clientY + 16) + "px";
      });
      node.addEventListener("mouseleave", () => {
        tooltip.style.display = "none";
      });
      node.addEventListener("click", () => {
        if (node.dataset.url) {
          window.open(node.dataset.url, "_blank", "noopener");
        }
      });
    });
  }

  function setOptions(id, values, includeAll) {
    const options = includeAll ? ["ALL"].concat(values) : values;
    byId(id).innerHTML = options.map((value) => (
      `<option value="${escapeAttr(value)}">${escapeHtml(value === "ALL" ? "All" : value)}</option>`
    )).join("");
  }

  function checkboxValues(id) {
    return new Set(Array.from(document.querySelectorAll(`#${id} input:checked`)).map((input) => input.value));
  }

  function recodeMethodGroup(value) {
    const text = clean(value).toLowerCase();
    if (text === "threshold" || text === "traditional" || text.includes("threshold")) return "Traditional";
    return "Non-traditional";
  }

  function isExternalValidation(row) {
    return /external/i.test(clean(row.validation));
  }

  function modelSourceText(row) {
    const source = clean(row.modelSource);
    if (!source) return "Not reported";
    if (/^no$/i.test(source)) return "New/not previously published in extraction";
    return source;
  }

  function isNotReported(value) {
    return /^(n\.?r\.?|not reported|na|n\/a)$/i.test(clean(value));
  }

  function tokenize(value) {
    return clean(value).split(";").map((part) => clean(part)).filter(Boolean);
  }

  function flatten(values) {
    return values.reduce((out, value) => out.concat(value), []);
  }

  function unique(values) {
    return Array.from(new Set(values.map(clean).filter(Boolean)));
  }

  function uniqueBy(rows, column) {
    const seen = new Set();
    return rows.filter((row) => {
      const key = clean(row[column]);
      if (!key || seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }

  function countValues(values) {
    const counts = new Map();
    values.map(clean).filter(Boolean).forEach((value) => counts.set(value, (counts.get(value) || 0) + 1));
    return Array.from(counts, ([name, count]) => ({ name, count })).sort((a, b) => b.count - a.count || a.name.localeCompare(b.name));
  }

  function groupBy(rows, fn) {
    return rows.reduce((groups, row) => {
      const key = fn(row);
      groups[key] = groups[key] || [];
      groups[key].push(row);
      return groups;
    }, {});
  }

  function riskLabel(value) {
    const text = clean(value);
    if (/low/i.test(text)) return "Low";
    if (/high/i.test(text)) return "High";
    return "Unclear";
  }

  function toNumber(value) {
    const text = clean(value);
    if (!text || /^n\.?r\.?$|^n\.?a\.?$|^na$|^nan$/i.test(text)) return NaN;
    const number = Number(text);
    return Number.isFinite(number) ? number : NaN;
  }

  function median(values) {
    const numbers = values.filter(Number.isFinite).sort((a, b) => a - b);
    if (!numbers.length) return NaN;
    const mid = Math.floor(numbers.length / 2);
    return numbers.length % 2 ? numbers[mid] : (numbers[mid - 1] + numbers[mid]) / 2;
  }

  function min(values) {
    const numbers = values.filter(Number.isFinite);
    return numbers.length ? Math.min(...numbers) : NaN;
  }

  function max(values) {
    const numbers = values.filter(Number.isFinite);
    return numbers.length ? Math.max(...numbers) : NaN;
  }

  function quantile(values, p) {
    const numbers = values.filter(Number.isFinite).sort((a, b) => a - b);
    if (!numbers.length) return NaN;
    const index = (numbers.length - 1) * p;
    const low = Math.floor(index);
    const high = Math.ceil(index);
    if (low === high) return numbers[low];
    return numbers[low] + (numbers[high] - numbers[low]) * (index - low);
  }

  function weightedAverage(values, weights) {
    let numerator = 0;
    let denominator = 0;
    values.forEach((value, index) => {
      const weight = weights[index];
      if (Number.isFinite(value) && Number.isFinite(weight) && weight > 0) {
        numerator += value * weight;
        denominator += weight;
      }
    });
    return denominator > 0 ? numerator / denominator : NaN;
  }

  function logit(value) {
    const p = Math.max(1e-6, Math.min(1 - 1e-6, value));
    return Math.log(p / (1 - p));
  }

  function invLogit(value) {
    return 1 / (1 + Math.exp(-value));
  }

  function clamp(value, low, high) {
    return Math.max(low, Math.min(high, value));
  }

  function iqrText(values) {
    return `IQR ${fmt(quantile(values, 0.25))} to ${fmt(quantile(values, 0.75))}`;
  }

  function pct(value, digits) {
    if (!Number.isFinite(value)) return "n.a.";
    return (value * 100).toFixed(digits == null ? 1 : digits) + "%";
  }

  function fmt(value) {
    if (!Number.isFinite(Number(value))) return "n.a.";
    return Number(value).toLocaleString("en-GB", { maximumFractionDigits: 1 });
  }

  function formatP(value) {
    if (!Number.isFinite(value)) return "n.a.";
    if (value < 0.001) return "<0.001";
    return value.toFixed(3);
  }

  function clean(value) {
    return String(value == null ? "" : value).trim();
  }

  function escapeHtml(value) {
    return clean(value).replace(/[&<>"']/g, (char) => ({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      "\"": "&quot;",
      "'": "&#039;"
    }[char]));
  }

  function escapeAttr(value) {
    return escapeHtml(value).replace(/`/g, "&#096;");
  }

  function formatCell(value) {
    const text = clean(value);
    if (/^https?:\/\//i.test(text)) {
      return `<a class="link-button" href="${escapeAttr(text)}" target="_blank" rel="noopener">${escapeHtml(truncate(text, 42))}</a>`;
    }
    if (/^10\.\S+\/\S+/.test(text)) {
      const url = "https://doi.org/" + text;
      return `<a class="link-button" href="${escapeAttr(url)}" target="_blank" rel="noopener">${escapeHtml(truncate(text, 42))}</a>`;
    }
    return escapeHtml(truncate(text, 120));
  }

  function truncate(value, length) {
    const text = clean(value);
    if (text.length <= length) return text;
    return text.slice(0, Math.max(0, length - 3)) + "...";
  }

  function metricSort(a, b) {
    return ["F1", "Sensitivity", "Specificity"].indexOf(a) - ["F1", "Sensitivity", "Specificity"].indexOf(b);
  }

  function domainSort(a, b) {
    const order = ["Sleep", "Activity type", "Activity intensity", "Energy expenditure"];
    return order.indexOf(a) - order.indexOf(b);
  }

  function formatDate(iso) {
    const date = new Date(iso);
    if (Number.isNaN(date.getTime())) return iso;
    return date.toLocaleString("en-GB", { dateStyle: "medium", timeStyle: "short" });
  }

  function safeJson(text) {
    try {
      return JSON.parse(text);
    } catch (error) {
      return null;
    }
  }

  function toCsv(rows) {
    if (!rows.length) return "";
    const cols = Object.keys(rows[0]);
    const quote = (value) => `"${clean(value).replace(/"/g, "\"\"")}"`;
    return [cols.map(quote).join(",")]
      .concat(rows.map((row) => cols.map((col) => quote(row[col])).join(",")))
      .join("\n");
  }
}());
