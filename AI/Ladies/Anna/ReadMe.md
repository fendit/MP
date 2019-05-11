# Anna - A prototype AI with Google tts recognition for Mac 

Hello everyone! This week I present you Anna, the AI I built with *Google Text-To-Speech Recognition*. She is based on Python and no worries! She is quite user-friendly! 

### Purposes
This script is to show that:
1. It is quite straightforward to create your own personal assistant
1. It is a good way to understand how personal assistant thinks (e.g. require keyword)
1. It is a cornerstone for future project, particularly when equiping with machine learning or deep learning

### Objectives
1. Create a personal assistant (Anna in this example)
1. Create ways to open application inside Mac with voice command
1. Create a fun dialogue with personal assistant (e.g. asking about her age)
1. Provide music with voice command
1. Understand how personal assistant (with certain keywords such as question words)
1. Convey a human-like conversation when the user claims not to be well
1. Provide search function (directly linked to Google search)
1. Provide random daily quotes

### prerequisite
1. It is only for **Mac** users
1. Make sure your mac connects to the Internet
1. Input your name and your birthday after executing the script (You can set user's name and birthday by default, which are "user" and "birthday" respectively)
1. Change the language if you want (please have a look at this: https://github.com/pndurette/gTTS/issues/31)

### Features
1. "How are you": Anna can respond to "how are you" question. She will ask the user the same question and wait for user's reply.
1. "What time is it": Anna can provide the current date and time.
1. "Open (Application)": Anna can open applications installed on your Mac.
1. "Play music": Anna can play music for you (by default I added a few songs with links directly linked to YouTube)
1. "Where is": Anna can search for places.
1. "Goodbye, Adios, Ciao": This can exit the programme.
1. "Happy Birthday": Anna will say "Happy Birthday" to you when today is your birthday
1. "Happy Cinco de Mayo": Anna will say so when is 5 May
1. "Random weekday quotes"
1. Show a list of previous commands (if any) when executing the current command

### Suggestions
1. Using offline voice package (e.g. pyttsx3)
1. Incorporate machine learning and deep learning
1. Having a more human-like conversation
1. Waiting for user to say certain keyword then activate (like Siri, Cortana and Alexa)
1. Creating a GUI

### References
Most of the lines are copied from various sources, but they are for Windows. I have modified in a simple and direct way on Mac.

1. https://www.youtube.com/watch?v=rU_ppVsyJu8 (A very good, detailed video for using alternative tts recognition)
2. https://www.youtube.com/watch?v=5mu6qlFY3x0&t=264s (Another great example for creating AI)
3. https://pythonspot.com/personal-assistant-jarvis-in-python/ (A great starting point if you are using Windows)
