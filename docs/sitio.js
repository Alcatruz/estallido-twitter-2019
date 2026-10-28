/* Utilidades compartidas por las páginas del sitio.
   Mismas convenciones que los otros posts: Plotly sobre los tokens del CSS,
   de modo que los gráficos siguen el modo claro u oscuro del navegador. */
(function () {
  "use strict";

  function num(x, d) {
    return Number(x).toLocaleString("es-CL", {
      minimumFractionDigits: d === undefined ? 0 : d,
      maximumFractionDigits: d === undefined ? 0 : d
    });
  }
  function pct(x, d) { return num(x, d === undefined ? 1 : d) + "%"; }
  function fecha(s, largo) {
    var m = ["ene", "feb", "mar", "abr", "may", "jun",
             "jul", "ago", "sep", "oct", "nov", "dic"];
    var p = s.split("-");
    var d = new Date(+p[0], +p[1] - 1, +p[2]);
    var dias = ["dom", "lun", "mar", "mié", "jue", "vie", "sáb"];
    var base = (+p[2]) + "-" + m[+p[1] - 1];
    return largo ? dias[d.getDay()] + " " + base : base;
  }

  function tok() {
    var s = getComputedStyle(document.documentElement);
    var g = function (n) { return s.getPropertyValue(n).trim(); };
    return {
      page: g("--page"), surface: g("--surface"), ink: g("--ink"),
      ink2: g("--ink-2"), muted: g("--muted"), grid: g("--grid"),
      axis: g("--axis"), izq: g("--izq"), der: g("--der"), sen: g("--sen"),
      neutro: g("--neutro"), link: g("--link")
    };
  }
  /* Paleta para series de más de tres categorías: los tres acentos de la casa
     y luego grises, para que nada compita con ellos. */
  function paleta(T) { return [T.izq, T.der, T.sen, T.neutro, T.ink2, T.muted]; }

  var CONFIG = { displayModeBar: false, responsive: true, scrollZoom: false };

  function ancho(id) {
    var e = document.getElementById(id);
    return e ? e.clientWidth : 680;
  }
  function angosto(id) { return ancho(id) < 560; }

  function base(T, extra) {
    var l = {
      paper_bgcolor: T.surface, plot_bgcolor: T.surface,
      font: { family: 'system-ui, -apple-system, "Segoe UI", Roboto, sans-serif', size: 12, color: T.ink2 },
      separators: ",.", margin: { l: 56, r: 16, t: 40, b: 48 },
      hoverlabel: { bgcolor: T.surface, bordercolor: T.axis, font: { color: T.ink, size: 13 } },
      legend: { orientation: "h", x: 0, xanchor: "left", y: 1.02, yanchor: "bottom", font: { color: T.ink2, size: 12 } },
      dragmode: false
    };
    for (var k in extra) l[k] = extra[k];
    return l;
  }
  function eje(T, extra) {
    var a = {
      gridcolor: T.grid, linecolor: T.axis, zeroline: false, fixedrange: true,
      tickfont: { color: T.muted }, title: { font: { color: T.ink2, size: 12 } }
    };
    for (var k in extra) { if (k === "title") a.title.text = extra[k]; else a[k] = extra[k]; }
    return a;
  }

  /* Construye una tabla: cols es [[encabezado, fn(fila), clase], ...] */
  function tabla(sel, cols, filas, destacar) {
    var n = document.querySelector(sel);
    if (!n) return;
    var th = "<tr>" + cols.map(function (c) {
      return '<th' + (c[2] ? ' class="' + c[2] + '"' : "") + ">" + c[0] + "</th>";
    }).join("") + "</tr>";
    var tb = filas.map(function (f, i) {
      var d = destacar && destacar(f, i) ? ' class="destacada"' : "";
      return "<tr" + d + ">" + cols.map(function (c) {
        var v = c[1](f, i);
        return "<td" + (c[2] ? ' class="' + c[2] + '"' : "") + ">" +
          (v === null || v === undefined || v === "" ? '<span class="vacio">·</span>' : v) + "</td>";
      }).join("") + "</tr>";
    }).join("");
    n.querySelector("thead").innerHTML = th;
    n.querySelector("tbody").innerHTML = tb;
  }

  /* Vuelve a dibujar cuando cambia el modo claro/oscuro o el tamaño.
     El evento de matchMedia no llega en todos los contextos -un visor
     embebido puede cambiar el esquema sin emitirlo-, así que además se
     compara el token de color cada tanto y se redibuja solo si cambió. */
  function alRedibujar(fn) {
    var t;
    function pronto() { clearTimeout(t); t = setTimeout(fn, 180); }
    fn();

    if (window.matchMedia) {
      var mq = window.matchMedia("(prefers-color-scheme: dark)");
      if (mq.addEventListener) mq.addEventListener("change", pronto);
    }
    if (window.MutationObserver) {
      new MutationObserver(pronto).observe(document.documentElement, {
        attributes: true, attributeFilter: ["data-theme", "class", "style"]
      });
    }
    var ultimo = getComputedStyle(document.documentElement).getPropertyValue("--surface");
    setInterval(function () {
      var v = getComputedStyle(document.documentElement).getPropertyValue("--surface");
      if (v !== ultimo) { ultimo = v; fn(); }
    }, 1200);

    window.addEventListener("resize", pronto);
    var lienzo = document.querySelector(".lienzo");
    if (lienzo && window.ResizeObserver) {
      var ancho0 = lienzo.clientWidth;
      new ResizeObserver(function () {
        if (Math.abs(lienzo.clientWidth - ancho0) < 2) return;
        ancho0 = lienzo.clientWidth;
        pronto();
      }).observe(lienzo);
    }
  }

  window.U = {
    num: num, pct: pct, fecha: fecha, tok: tok, paleta: paleta,
    CONFIG: CONFIG, ancho: ancho, angosto: angosto, base: base, eje: eje,
    tabla: tabla, alRedibujar: alRedibujar
  };
})();
