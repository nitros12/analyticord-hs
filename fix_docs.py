import re
import glob

doc_link = re.compile("<a href=\"\.{2}\/([\w.-]+)\/([#:\w.-]*)\">")

files = glob.glob("docs/*.html")
print("Replacing links for files: {}".format("\n".join(files)))

for fname in files:
    with open(fname) as f:
        data = f.read()
    data = doc_link.sub(
        r'<a href="https://hackage.haskell.org/package/\1/docs/\2">', data)
    with open(fname, "w") as f:
        f.write(data)
print("finished replacing docs.")
