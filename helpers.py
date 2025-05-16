import requests
import json
from dotenv import load_dotenv
import os
import glob


load_dotenv()

def get_a_response(prompt, model="mistral-nemo-instruct-2407", api_key=os.getenv("MISTRAL_API_KEY"), history=[]):
    URL = os.getenv("MISTRAL_API_URL")
    

    print(f"The history received in the helper function {history}")

    history.append({"role": "user", "content" : prompt})

    print (f"Updated History : {history}")

    HEADERS = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }
    
    PAYLOAD = {
        "model": model,
        "messages": [{"role": "user", "content": prompt}], #change this later when we figure out how the history could be implemented
        "max_tokens": 512,
        "temperature": 0, #control the response randomness
        "top_p": 1,
        "presence_penalty": 0,
        "stream": True,
    }
    
    response = requests.post(URL, headers=HEADERS, data=json.dumps(PAYLOAD), stream=True)
    
    output = ""
    for line in response.iter_lines():
        if line:
            decoded_line = line.decode('utf-8').strip()
            if decoded_line == "data: [DONE]":
                break
            if decoded_line.startswith("data: "):
                try:
                    data = json.loads(decoded_line[len("data: "):])
                    if data.get("choices") and data["choices"][0]["delta"].get("content"):
                        output += data["choices"][0]["delta"]["content"]
                except json.JSONDecodeError:
                    continue

    
    
    return output




def merge_pdfs():
    import fitz
    folder_path = "./data/raw"
    pdf_files = glob.glob(folder_path + "/*.pdf")

    print(len(pdf_files))

    merged_pdf = fitz.open()

    for pdf in pdf_files:
        with fitz.open(pdf) as mydoc:
            merged_pdf.insert_pdf(mydoc)


    merged_pdf.save("./data/merged/merged.pdf")   
