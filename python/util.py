import requests

PROJECT_FOLDER = "/Users/hongkaiyu/Developer/macro-econ"


def download_file(url, save_path):
    response = requests.get(url)
    if response.status_code == 200:
        with open(save_path, 'wb') as file:
            file.write(response.content)
        print("File downloaded successfully.")
    else:
        print("Failed to download the file.")
