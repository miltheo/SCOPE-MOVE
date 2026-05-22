const fs = require("fs");
const path = require("path");

const appDir = __dirname;
const root = path.resolve(appDir, "..");

const csvFiles = {
  classifierInput: "analysis/inputs/Extraction_Classifiers.csv",
  energyInput: "analysis/inputs/Extraction_EnergyExpenditure.csv",
  studyCharacteristics: "analysis/inputs/Study_Characteristics.csv",
  qualityAssessment: "analysis/inputs/Quality_Assessment.csv",
  appRemlLong2: "app/data/REML_data_long2.csv"
};

const qualityToolFile = "app/data/Quality_Assessment_Tool.csv";

const docs = {
  readme: "README.md",
  appReadme: "app/README.md",
  appChangelog: "app/CHANGELOG.md",
  changelog: "CHANGELOG.md",
  citation: "CITATION.cff",
  zenodo: ".zenodo.json",
  dataDictionary: "analysis/inputs/Data_Dictionary.pdf"
};

function abs(rel) {
  return path.join(root, rel);
}

function readText(rel) {
  return fs.readFileSync(abs(rel), "utf8");
}

function parseCsvRows(text) {
  text = text.replace(/^\uFEFF/, "");
  const rows = [];
  let row = [];
  let cell = "";
  let quoted = false;

  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    const next = text[i + 1];

    if (quoted) {
      if (char === "\"" && next === "\"") {
        cell += "\"";
        i += 1;
      } else if (char === "\"") {
        quoted = false;
      } else {
        cell += char;
      }
    } else if (char === "\"") {
      quoted = true;
    } else if (char === ",") {
      row.push(cell);
      cell = "";
    } else if (char === "\n") {
      row.push(cell);
      rows.push(row);
      row = [];
      cell = "";
    } else if (char !== "\r") {
      cell += char;
    }
  }

  if (cell.length || row.length) {
    row.push(cell);
    rows.push(row);
  }

  return rows;
}

function parseCsv(text) {
  const rows = parseCsvRows(text);
  if (!rows.length) {
    return [];
  }

  const headers = rows[0].map((header) => header.trim());
  return rows.slice(1)
    .map((values) => {
      const record = {};
      headers.forEach((header, index) => {
        record[header] = values[index] == null ? "" : values[index];
      });
      return record;
    });
}

function parseQualityAssessmentTool(text) {
  const rows = parseCsvRows(text);
  const questions = [];
  const riskRules = [];
  let currentDomain = "";

  rows.forEach((cells) => {
    const item = clean(cells[0]);
    const second = clean(cells[1]);
    const instructions = clean(cells[2]);
    const responseScale = clean(cells[3]);

    if (item === "Item" && /^Domain\s+\d+/i.test(second)) {
      currentDomain = second;
      return;
    }

    if (/^\d+$/.test(item) && second) {
      questions.push({
        item: `SQ${item}`,
        domain: currentDomain,
        question: second,
        instructions,
        responseScale: responseScale || "Yes/No/Unclear/NA"
      });
      return;
    }

    if (/^Could\b/i.test(second) && instructions && currentDomain) {
      riskRules.push({
        domain: currentDomain,
        question: second,
        instructions,
        responseScale: responseScale || "Low/High/Unclear"
      });
    }
  });

  return {
    source: "app/data/Quality_Assessment_Tool.csv",
    questions,
    riskRules
  };
}

function toNumber(value) {
  if (value == null) return NaN;
  const text = String(value).trim();
  if (!text || /^n\.?r\.?$|^n\.?a\.?$|^na$|^nan$/i.test(text)) return NaN;
  const number = Number(text);
  return Number.isFinite(number) ? number : NaN;
}

function clean(value) {
  return String(value == null ? "" : value).trim();
}

function uniqueValues(rows, column) {
  return Array.from(new Set(rows.map((row) => clean(row[column])).filter(Boolean)));
}

function countRows(rows, predicate) {
  return rows.reduce((sum, row) => sum + (predicate(row) ? 1 : 0), 0);
}

function countBy(rows, column) {
  const counts = {};
  rows.forEach((row) => {
    const value = clean(row[column]) || "(blank)";
    counts[value] = (counts[value] || 0) + 1;
  });
  return Object.entries(counts)
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .map(([name, count]) => ({ name, count }));
}

function clamp(value, lower, upper) {
  return Math.min(Math.max(value, lower), upper);
}

function serializeNumber(value) {
  if (!Number.isFinite(value)) return "";
  const normalized = Object.is(value, -0) ? 0 : value;
  return Number(normalized.toPrecision(12)).toString();
}

function isUnitIntervalMetric(row) {
  return /^(f1|sensitivity|specificity)$/i.test(clean(row.metric));
}

function normalizeCiIntervals(rows) {
  return rows.map((row) => {
    const low = toNumber(row.ci_l);
    const est = toNumber(row.est);
    const high = toNumber(row.ci_u);

    if (!Number.isFinite(low) || !Number.isFinite(est) || !Number.isFinite(high)) {
      return row;
    }

    let normalizedLow = Math.min(low, est);
    let normalizedHigh = Math.max(high, est);

    if (isUnitIntervalMetric(row)) {
      normalizedLow = clamp(normalizedLow, 0, 1);
      normalizedHigh = clamp(normalizedHigh, 0, 1);
    }

    if (normalizedLow === low && normalizedHigh === high) {
      return row;
    }

    return {
      ...row,
      ci_l: serializeNumber(normalizedLow),
      ci_u: serializeNumber(normalizedHigh)
    };
  });
}

function numericStats(rows, column) {
  const values = rows.map((row) => toNumber(row[column])).filter(Number.isFinite).sort((a, b) => a - b);
  if (!values.length) {
    return { n: 0, missing: rows.length, min: null, max: null };
  }
  const q = (p) => {
    const index = (values.length - 1) * p;
    const low = Math.floor(index);
    const high = Math.ceil(index);
    if (low === high) return values[low];
    return values[low] + (values[high] - values[low]) * (index - low);
  };
  return {
    n: values.length,
    missing: rows.length - values.length,
    min: values[0],
    q1: q(0.25),
    median: q(0.5),
    q3: q(0.75),
    max: values[values.length - 1]
  };
}

function duplicateKeys(rows, column) {
  return countBy(rows, column).filter((entry) => entry.name !== "(blank)" && entry.count > 1);
}

const records = {};
Object.entries(csvFiles).forEach(([key, rel]) => {
  records[key] = parseCsv(readText(rel));
});

const hiddenColumns = new Set(["Reported?", "CM?", "other", "dynami_range"]);
Object.keys(records).forEach((key) => {
  records[key] = records[key].map((row) => {
    const cleaned = {};
    Object.entries(row).forEach(([column, value]) => {
      if (!hiddenColumns.has(column)) cleaned[column] = value;
    });
    return cleaned;
  });
});

records.appRemlLong2 = normalizeCiIntervals(records.appRemlLong2);

const classifier = records.classifierInput;
const reml = records.appRemlLong2;
const energy = records.energyInput;
const studies = records.studyCharacteristics;
const quality = records.qualityAssessment;

const classifierMetricOutside01 = ["f1_mean", "sens_mean", "spec_mean"].reduce((sum, column) => (
  sum + countRows(classifier, (row) => {
    const value = toNumber(row[column]);
    return Number.isFinite(value) && (value < 0 || value > 1);
  })
), 0);

const remlCiIssues = reml
  .filter((row) => {
    const low = toNumber(row.ci_l);
    const est = toNumber(row.est);
    const high = toNumber(row.ci_u);
    return Number.isFinite(low) && Number.isFinite(est) && Number.isFinite(high) && !(low <= est && est <= high);
  })
  .map((row) => ({
    val_id: clean(row.val_id),
    metric: clean(row.metric),
    est: toNumber(row.est),
    ci_l: toNumber(row.ci_l),
    ci_u: toNumber(row.ci_u)
  }));

const blankEnergyRows = energy.filter((row) => !clean(row.id) && !clean(row.val_id) && !clean(row.study));

const checks = {
  sourceRowCounts: Object.fromEntries(Object.entries(records).map(([key, rows]) => [key, rows.length])),
  sourceColumnCounts: Object.fromEntries(Object.entries(records).map(([key, rows]) => [key, rows[0] ? Object.keys(rows[0]).length : 0])),
  duplicateKeys: {
    classifierId: duplicateKeys(classifier, "id").slice(0, 12),
    classifierValId: duplicateKeys(classifier, "val_id").slice(0, 12),
    studyId: duplicateKeys(studies, "id").slice(0, 12),
    qualityStudyId: duplicateKeys(quality, "Study ID").slice(0, 12)
  },
  canonicalFiles: {
    reproducibleSources: "analysis/inputs",
    appValidationTable: csvFiles.appRemlLong2
  },
  classifierMetricOutside01,
  classifierMetricUnitInference: classifierMetricOutside01 > 0 ? "percent_0_100" : "proportion_0_1",
  classifierMissingDoiRows: countRows(classifier, (row) => !clean(row.doi)),
  remlCiOrderIssues: remlCiIssues,
  energyBlankRows: blankEnergyRows.length,
  energyRowsWithMape: countRows(energy, (row) => Number.isFinite(toNumber(row.mape_mean))),
  studyYearRange: numericStats(studies, "year"),
  classifierMetrics: {
    f1_mean: numericStats(classifier, "f1_mean"),
    sens_mean: numericStats(classifier, "sens_mean"),
    spec_mean: numericStats(classifier, "spec_mean")
  },
  remMetrics: {
    est: numericStats(reml, "est"),
    vi_use: numericStats(reml, "vi_use")
  },
  energyMetrics: {
    mape_mean: numericStats(energy, "mape_mean"),
    rmse_met_mean: numericStats(energy, "rmse_met_mean")
  },
  topValues: {
    classifierDomain: countBy(classifier, "movement_behaviour").slice(0, 12),
    classifierMethod: countBy(classifier, "method").slice(0, 12),
    studyCountry: countBy(studies, "country").slice(0, 12),
    qualityDomain1: countBy(quality, "Domain 1: Patient Selection/Study Design")
  }
};

const docBundle = Object.fromEntries(Object.entries(docs).map(([key, rel]) => {
  if (rel.endsWith(".pdf")) {
    const stat = fs.statSync(abs(rel));
    const url = rel.startsWith("app/")
      ? rel.slice(4)
      : `https://github.com/miltheo/SCOPE-MOVE/blob/main/${rel}`;
    return [key, { path: rel, url, bytes: stat.size }];
  }
  return [key, { path: rel, text: readText(rel) }];
}));

const payload = {
  generatedAt: new Date().toISOString(),
  sources: { ...csvFiles, qualityAssessmentTool: qualityToolFile },
  records,
  qualityAssessmentTool: parseQualityAssessmentTool(readText(qualityToolFile)),
  docs: docBundle,
  checks,
  summaries: {
    classifierRows: classifier.length,
    classifierValidationIds: uniqueValues(classifier, "val_id").length,
    classifierStudies: uniqueValues(classifier, "id").length,
    classifierStudyLabels: uniqueValues(classifier, "study").length,
    classifierStudyIds: uniqueValues(classifier, "id").length,
    remlRows: reml.length,
    energyRows: energy.length,
    energyAnalyticRows: energy.length - blankEnergyRows.length,
    studyRows: studies.length,
    countries: uniqueValues(studies, "country").length,
    qualityRows: quality.length
  }
};

const js = "window.SCOPE_MOVE_DATA = " + JSON.stringify(payload, null, 2) + ";\n";
fs.writeFileSync(path.join(appDir, "data.js"), js, "utf8");
console.log(`Wrote app/data.js (${Math.round(js.length / 1024)} KB)`);
