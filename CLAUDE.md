## dseval

This repository implements a variant of DSBench, a data science LLM evaluation, compatible with [Inspect AI](https://inspect.aisi.org.uk/) and [vitals](https://vitals.tidyverse.org/).

The original DSBench source code is in `DSBench/`.

## DSBench Data Analysis Original Evaluation Structure

The original DSBench is implemented as follows:

Data Organization:
```
DSBench/data_analysis/
├── data.json                  # Competition metadata
├── data/{competition_id}/     # Individual competition folders
│   ├── introduction.txt       # Background scenario
│   ├── questionX.txt          # Individual questions
│   ├── *.xlsx                 # Excel workbooks (data tables)
│   └── *.jpg                  # Images (optional)
```

Evaluation Flow (eval_GPT.ipynb):

Competition Loading:

```python
# Load competition metadata
samples = []
with open("./data.json", "r") as f:
    for line in f:
        samples.append(eval(line.strip()))

# Each sample contains:
# {"id": "00000001", "questions": ["question6", "question7", ...], 
#  "answers": ["D", "D", "I", ...], "name": "...", "year": 2016}
```

File Discovery (per competition):

```python
# Find data files in competition directory
image = find_jpg_files(os.path.join(data_path, sample["id"]))
excels = find_excel_files(os.path.join(data_path, sample["id"]))

# Excel filter: exclude files with "answer" in filename
jpg_files = [file for file in os.listdir(directory) 
             if file.lower().endswith('.jpg') or file.lower().endswith('.png')]
excel_files = [file for file in os.listdir(directory) 
               if (file.lower().endswith('xlsx') or file.lower().endswith('xlsb') or file.lower().endswith('xlsm'))
               and not "answer" in file.lower()]
```

Content Processing:

```python
# Process Excel files
excel_content = ""
for excel in excels:
    sheets = read_excel(excel_file_path)  # pd.ExcelFile, parse all sheets
    combined_text = combine_sheets_text(sheets)  # df.to_string(index=False)
    excel_content += f"The excel file {excel} is: " + combined_text

# Read introduction
introduction = read_txt(os.path.join(data_path, sample["id"], "introduction.txt"))

# Read individual questions
questions = []
for question_name in sample["questions"]:
    questions.append(read_txt(os.path.join(data_path, sample["id"], question_name+".txt")))
```

Prompt Construction (per question):

```python
# Base context (same for all questions in competition)
text = ""
if excel_content:
    text += f"The workbook is detailed as follows. {excel_content} \n"
text += f"The introduction is detailed as follows. \n {introduction} \n"

# Individual question prompt
for question in questions:
    prompt = text + f"The questions are detailed as follows. \n {question}"
    
    # Token limiting
    cut_text = encoding.decode(encoding.encode(prompt)[tokens4generation-MODEL_LIMITS[model]:])
```

Model Call:

```python
# System prompt
system_prompt = "You are a data analyst. I will give you a background introduction and data analysis question. You must answer the question."

# Vision support
if image and model[:7] != "gpt-3.5":
    base64_image = encode_image(image)
    # Include image in message content
    
# Parameters
temperature=0, max_tokens=2256, top_p=1, frequency_penalty=0, presence_penalty=0
```

Response Processing:

```python
# Extract and save response
response = get_gpt_res(cut_text, image, model)
answer = {
    "id": sample["id"], 
    "model": response.model,
    "input": response.usage.prompt_tokens,
    "output": response.usage.completion_tokens, 
    "cost": cost, 
    "time": time.time()-start, 
    "response": response.choices[0].message.content
}

# Save to ./save_process/{model}/{competition_id}.json
```

Key Implementation Details:

- **Each question evaluated independently** with full context
- **Context = Excel data + introduction + individual question**. The excel file is read in and provided as text.
- **Images included if present and model supports vision**
- **Token truncation from beginning** if prompt exceeds model limits
- **Answers stored separately** from evaluation responses
- **Ground truth** from data.json answers array (index-matched to questions array)
- **No conversation history** between questions
