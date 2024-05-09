"""
Parse the Journal Impact Factors from a PDF
"""

import tabula

def parse_table(path):

    df_raw = tabula.read_pdf(path, pages="all", guess=False, columns=[340], multiple_tables=False)[0]
    df = df_raw.rename(columns={df_raw.columns[0]: "journal_name", df_raw.columns[1]: "jif_2022"})
    df = df.iloc[:-2]
    df["jif_2022"] = df["jif_2022"].str.extract('(\d+)')

    return df

def write_to_file(df, path):

    df.to_csv(path, index=False)


def main():
    pdf_path = "./data/JournalImpactFactors2023.pdf"
    save_path = "./data/journal_impact_factors_2022.csv"

    df = parse_table(pdf_path)
    write_to_file(df, save_path)

if __name__ == "__main__":
    main()