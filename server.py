from flask import Flask, request, jsonify
import os
app = Flask(__name__)

from rag import RAGPipeline


        # Path to the `data` folder
data_folder = "data"
os.makedirs(data_folder, exist_ok=True)  # Create the folder if it doesn't exist
pdf_path = "manuel_plombier.pdf"  
rag_system = RAGPipeline(pdf_path)


print("The server file ...")

import os
import json

@app.route('/chat', methods=['POST'])
def rag_endpoint():
    print("Chat endpoint")
    try:
        # Parse the request payload
        data = request.get_json()

        # Extract query and email
        user_query = data.get("query")
        user_email = data.get("email")

        # Validate inputs
        if not user_query or not user_email:
            return jsonify({"error": "Missing query or email"}), 400



        # Path to the user's JSON history file
        user_history_path = os.path.join(data_folder, f"{user_email}.json")

        # Load or initialize the conversation history
        if os.path.exists(user_history_path):
            with open(user_history_path, "r") as history_file:
                conversation_history = json.load(history_file)
        else:
            conversation_history = []  # Start with an empty history
            with open(user_history_path, "w") as history_file:
                json.dump(conversation_history, history_file)

        
        print(f"Conversation history before response {conversation_history}")

        # Provide the conversation history to the query_pipeline
        response = rag_system.query_pipeline(user_query, conversation_history.copy())

        print(f"The conversation history afet getting the response from the RAG {conversation_history}")



        # Update the conversation history
        conversation_history.append({
            "role": "user",
            "content": user_query
        })
        conversation_history.append({
            "role": "system",
            "content": response["response"]
        })

        # Save the updated history
        with open(user_history_path, "w") as history_file:
            json.dump(conversation_history, history_file)

        # Return the response
        return jsonify({
            "response": response["response"],
            "pages" : response["retrieved_docs"]
        })

    except Exception as e:
        print(f"Error occurred: {e}")
        return jsonify({"error": str(e)}), 500


@app.route("/health")
def home():
    return {"message" : "The API is working"}



@app.route("/history", methods=['GET'])
def get_history():
    try:
        user_email = request.args.get("email")

        if not user_email:
            return jsonify({"error": "Missing email"}), 400

        user_history_path = os.path.join(data_folder, f"{user_email}.json")

        if os.path.exists(user_history_path):
            with open(user_history_path, "r") as history_file:
                conversation_history = json.load(history_file)
        else:
            conversation_history = []

        return jsonify({"history": conversation_history})

    except Exception as e:
        print(f"Error occurred: {e}")
        return jsonify({"error": str(e)}), 500




if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)  # Change port if needed
