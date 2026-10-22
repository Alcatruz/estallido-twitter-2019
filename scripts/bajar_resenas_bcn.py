import urllib.request, urllib.parse, re, time, csv, os, collections

BASE = "https://www.bcn.cl/historiapolitica/"
UA = {"User-Agent": "estallido-data-research/0.1 (academic)"}
slugs = [l.strip() for l in open("slugs.txt", encoding="utf-8") if l.strip()]

patron = re.compile(r"(?:twitter|x)\.com/(?:#!/)?@?([A-Za-z0-9_]{1,15})", re.I)
basura = {"share", "intent", "home", "search", "hashtag", "i", "bcnchile", "bcn_chile"}

filas, por_handle = [], collections.Counter()
for k, s in enumerate(slugs, 1):
    nombre = s.split("/wiki/")[1]
    ruta = os.path.join("resenas", nombre.replace("/", "_") + ".html")
    if not os.path.exists(ruta):
        url = BASE + urllib.parse.quote(s)
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=30) as r:
                html = r.read().decode("utf-8", "replace")
            open(ruta, "w", encoding="utf-8").write(html)
        except Exception as e:
            filas.append({"slug": nombre, "estado": f"error: {e}", "handles": ""})
            continue
        time.sleep(0.6)
    html = open(ruta, encoding="utf-8").read()
    hs = sorted({h for h in patron.findall(html) if h.lower() not in basura})
    por_handle.update(h.lower() for h in hs)
    filas.append({"slug": nombre, "estado": "ok", "handles": "|".join(hs)})
    if k % 25 == 0:
        print(f"  {k}/{len(slugs)}", flush=True)

# Un handle que aparece en casi todas las paginas es del sitio, no del diputado
comunes = {h for h, n in por_handle.items() if n > len(slugs) * 0.5}
print("handles de plantilla del sitio (descartados):", comunes)
for f in filas:
    f["handles"] = "|".join(h for h in f["handles"].split("|") if h and h.lower() not in comunes)

with open("bcn_twitter.csv", "w", encoding="utf-8", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=["slug", "estado", "handles"]); w.writeheader(); w.writerows(filas)

ok = [f for f in filas if f["estado"] == "ok"]
con = [f for f in ok if f["handles"]]
multi = [f for f in con if "|" in f["handles"]]
print(f"paginas ok: {len(ok)}/{len(filas)} | con cuenta X: {len(con)} | con mas de una: {len(multi)}")
for f in multi[:10]:
    print("   ", f["slug"], "->", f["handles"])
