import pandas as pd
import requests
import tabula


PROJECT_FOLDER = "/Users/hongkaiyu/Developer/macro-econ"


def download_file(url, save_path):
    response = requests.get(url)
    if response.status_code == 200:
        with open(save_path, 'wb') as file:
            file.write(response.content)
        print("File downloaded successfully.")
    else:
        print("Failed to download the file.")

def extract_tables_from_pdf(pdf_path) -> list[pd.DataFrame]:
    """
    return a list of tables
    """
    try:
        # Read the PDF and extract tables
        tables = tabula.read_pdf(pdf_path, pages='all', multiple_tables=True)

        # Return the extracted tables
        return tables

    except Exception as e:
        print(f"An error occurred while extracting tables from the PDF: {str(e)}")
        return []
