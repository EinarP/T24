from bs4 import BeautifulSoup
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer
from reportlab.lib.units import cm
from xml.sax.saxutils import escape  # To safely escape problematic characters
import os

# Input and output parameters
root_folder = "D:/Work/R25/HelpText/HelpText"
output_pdf = "Temenos_Helptext_Combined.pdf"

# Fetch all files recursively
def get_all_xml_files(root_dir):
    xml_files = []
    for dirpath, _, filenames in os.walk(root_dir):
        for f in filenames:
            if f.lower().endswith(".xml"):
                xml_files.append(os.path.join(dirpath, f))

    return sorted(xml_files)

# Parser function
def parse_t24_helptext(xml_path):
    with open(xml_path, "r", encoding="utf-8") as f:
        print("Processing:", xml_path)
        soup = BeautifulSoup(f, "lxml-xml")

    product = soup.find("product").text if soup.find("product") else "UNKNOWN"
    table = soup.find("table").text if soup.find("table") else os.path.basename(xml_path)
    overview = soup.find("overview").get_text(separator="\n", strip=True) if soup.find("overview") else ""

    fields = []
    for t in soup.find_all("t"):
        field_name = t.find("field").text if t.find("field") else "UNKNOWN"
        desc = t.find("desc").get_text(separator="\n", strip=True) if t.find("desc") else ""
        fields.append((field_name, desc))

    return {
        "product": product,
        "table": table,
        "overview": overview,
        "fields": fields
    }

# Grouping by product
data_by_product = {}
for file_path in get_all_xml_files(root_folder):
    parsed = parse_t24_helptext(file_path)
    product = parsed["product"]
    if product not in data_by_product:
        data_by_product[product] = []
    data_by_product[product].append(parsed)

# Setup PDF
doc = SimpleDocTemplate(output_pdf, pagesize=A4, rightMargin=2*cm, leftMargin=2*cm, topMargin=2*cm, bottomMargin=2*cm)

styles = getSampleStyleSheet()
styles.add(ParagraphStyle(name='CustomHeading1', fontSize=16, leading=20, spaceAfter=12))
styles.add(ParagraphStyle(name='CustomHeading2', fontSize=14, leading=18, spaceAfter=10))
styles.add(ParagraphStyle(name='CustomFieldName', fontSize=12, leading=14, spaceAfter=6, spaceBefore=6, leftIndent=10, fontName="Helvetica-Bold"))
styles.add(ParagraphStyle(name='CustomFieldDesc', fontSize=11, leading=14, spaceAfter=4, leftIndent=15))

# Build content
content = []
for product, applications in data_by_product.items():
    content.append(Paragraph(f"Product: {escape(product)}", styles['CustomHeading1']))
    
    for app in applications:
        content.append(Paragraph(f"Application: {escape(app['table'])}", styles['CustomHeading2']))
        
        if app["overview"]:
            content.append(Paragraph(f"<b>Overview:</b><br/>{escape(app['overview'])}", styles['CustomFieldDesc']))
        
        content.append(Paragraph("Fields:", styles['CustomHeading2']))
        
        for field_name, desc in app["fields"]:
            if field_name and desc:
                safe_field = escape(field_name)
                safe_desc = escape(desc)
                content.append(Paragraph(safe_field, styles['CustomFieldName']))
                content.append(Paragraph(safe_desc, styles['CustomFieldDesc']))
        
        content.append(Spacer(1, 12))

# Generate PDF
doc.build(content)
print(f"✅ PDF generated: {output_pdf}")