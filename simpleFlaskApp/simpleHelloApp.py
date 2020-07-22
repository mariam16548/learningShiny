from flask import Flask, render_template, request 

app = Flask(__name__)

@app.route("/", methods=["GET", "POST"]) 
def index(): 
	note = request.form.get("note")
	if note is None: 
		return render_template("hello.html")			
	else:
		return render_template("index.html", note=note)
		
if __name__ == "__main__":
   app.run(host="0.0.0.0", debug=True)
