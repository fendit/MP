from gtts import gTTS
import datetime
import os
import pandas as pd
import random
import speech_recognition as sr
import time


# User's information
user = input("What's your name? :")
birthday = input("When is your birthday? (MM-DD):")



# Create an empty log
df = pd.DataFrame(columns=['Time', 'Data'])
today = datetime.datetime.today().strftime('%m-%d')
# today = "05-04"   # Surprise
today_weekday = datetime.datetime.today().weekday()


def speak(audiostring):
    print(audiostring)
    tts = gTTS(text=audiostring, lang='en')
    tts.save("anna.mp3")
    os.system('afplay anna.mp3')


# Voice Input
def recordAudio():
    r = sr.Recognizer()
    with sr.Microphone() as source:
        print('I am listening to you now.')
        audio = r.listen(source)

    data = ''
    try:
        # Print out the voice input
        data = r.recognize_google(audio)
        print("You said: " + data)
    except sr.UnknownValueError:
        print("I don't understand what you've just said, Sorry.")
    except sr.RequestError as e:
        print("Could not request results from Google Speech Recognition service; {0}".format(e))

    return data


question_tag = ['what', 'when', 'who', 'why', 'how', 'how far', 'how long', 'how many', 'how much', 'how old',
                'how come', 'show me']

music_selection = ['i6iBAuwBODA',   # Bee Gees - Too Much Heaven
                   'kXYiU_JCYtU',   # Linkin Park - Numb
                   'eVTXPUF4Oz4',   # Linkin Park - In the End
                   'UBgAj4cNee4',   # Bee Gees - Emotion
                   'fJ9rUzIMcZQ',   # Queen - Bohemian Rhapsody
                   'Gs069dndIYk'    # Earth, Wind & Fire - September
                   ]
pos_repsonse = ["okay", "ok", "fine", "alright", "happy", "excited", "great"]
neg_response = ["terrible", "not so good", "bad", "down", "I want to cry"]
goodbye = ["goodbye", "adios", "ciao"]


def anna(data):
    if "how are you" in data:
        speak(random.choice(greetings_response) + " Thank you. How about you?")
        query = recordAudio()

        if any(word in query for word in pos_repsonse):
            speak("I am glad to hear that.")

        elif any(word in query for word in neg_response):
            speak("Albert Einstein once said,\n"
                  "'In the middle of difficulty lies opportunity.'")

        else:
            pass

    elif "what time is it" in data:
        speak(time.ctime())

    elif "how old are you" in data:
        speak("Mark Twain once said,\n"
              "'Age is an issue of mind over matter. If you don’t mind, it doesn’t matter.'")

    elif "where is" in data:
        data = data.split(" ")

        if len(data) > 3:
            location = '+'.join(data[2:len(data)])
            position = ' '.join(data[2:len(data)])
        else:
            location = data[2]
            position = data[2]

        speak("Hold on Fendi, I will show you where " + position + " is.")
        os.system("open -a /Applications/safari.app https://www.google.com/maps/place/" + location)
        time.sleep(10)

    elif 'open' in data:
        data = data.split(" ")

        if len(data) > 2:
            app_name_address = '\ '.join(data[1:len(data)])
            app_name = ' '.join(data[1:len(data)])
        else:
            app_name_address = data[1]
            app_name = data[1]

        speak("Wait a minute, " + user + ". I am about to open " + app_name + ".")
        os.system("open -a /Applications/" + app_name_address + ".app/")
        time.sleep(10)

    elif any(word in data for word in question_tag):
        data = data.split(" ")

        if len(data) > 3:
            search_index = '+'.join(data[2:len(data)])
            search_name = ' '.join(data[2:len(data)])
        else:
            search_index = data[2]
            search_name = data[2]

        speak("Just a second, " + user + ". I am about to search for " + search_name + ".")
        os.system("open -a /Applications/safari.app https://www.google.com/search?q=" + search_index)
        time.sleep(10)

    elif 'music' in data:
        speak("Just a moment, " + user + ".")
        os.system("open -a /Applications/safari.app https://www.youtube.com/watch?v=" + random.choice(music_selection))
        time.sleep(300)

    elif any(word in data for word in goodbye):
        speak("Goodbye, " + user + ". Have a nice day.")
        exit()

    time.sleep(3)
    speak("Is there anything I can help?")
    query = recordAudio()
    if "yes" in query:
        speak("So, how can I help?")
    elif "no" in query:
        speak("Alright. Goodbye, Fendi. Have a nice day.")
        exit()


speak("Hello, " + user + ". I am Anna.")

if today == birthday:
    speak("Happy Birthday to you, " + user + ". I wish you all the best.")
# elif today == "05-04":
#     os.system("afplay May_da_4.mp3")
elif today == "05-05":
    speak("Happy Cinco De Mayo!")
else:
    pass

mon_quotes = ["Good morning. Keep calm and pretend it’s not Monday.",
              "May your coffee be strong and your Monday productive.",
              "Dear Monday, I want to break up.\n"
              "I’m seeing Tuesday and dreaming about Friday.\n"
              "It’s not me, it’s you."]
tue_quotes = ["Happy Tuesday. Don’t worry Friday is coming.",
              "Tuesday’s are really just Monday’s dressed in their Sunday best.",
              "Tuesday is the day I actually start the week,\n"
              "Monday I just deal with the depression of the weekend ending."]
wed_quotes = ["William Shakespeare once said,\n"
              "Happy Wednesday! Love all, trust a few, do wrong to none.",
              "Keep calm, its hump day!",
              "Keep Calm. It’s only Wednesday. We still have 2 more days to go."]
thu_quotes = ["Happy Day before Friday!",
              "Some people call it Thursday, I like to call it Friday Eve.",
              "If 40 is the new 30, and 50 is the new 40,\n"
              "why can’t Thursday be the new Friday?"]
fri_quotes = ["Friday called. She’s on her way and she’s bringing the wine.",
              "Have a fabulous Friday, Darlings!",
              "TGIF. Thank God I’m Female. Thank God I’m Fabulous.\n"
              "Thank God I’m Funny."
              "And yes, Thank God It’s Friday!"]
sat_quotes = ["Saturday is here;\n"
              "give it a warm welcome by allowing yourself a lovely day of rest.",
              "Today is Saturday,\n"
              " which means that today I will be multi-slacking instead of\n"
              " multi-tasking",
              "Have a Happy Saturday. Saturdays are for adventures,\n"
              " Sundays are for cuddling."]
sun_quotes = ["May your Sunday be blessed with love, joy, peace and happiness.",
              "On this lovely Sunday, remember to take a deep breath and relax.\n"
              "Enjoy your family, your friends,\n"
              "and indulge yourself in a nice cup of coffee.",
              "If your Sunday doesn’t involve wine & yoga pants,\n"
              " you’re doing it wrong."]

if today_weekday == 0:  # Monday
    speak(random.choice(mon_quotes))
elif today_weekday == 1:   # Tuesday
    speak(random.choice(tue_quotes))
elif today_weekday == 2:   # Wednesday
    speak(random.choice(wed_quotes))
elif today_weekday == 3:   # Thursday
    speak(random.choice(thu_quotes))
elif today_weekday == 4:   # Friday
    speak(random.choice(fri_quotes))
elif today_weekday == 5:   # Saturday
    speak(random.choice(sat_quotes))
else:   # Sunday
    speak(random.choice(sun_quotes))

speak("What can I do for you, " + user + "?")

greetings_response = ["I am fine.",
                      "I am okay.",
                      "Overworked and underpaid.",
                      "Nothing much.",
                      "I don't know, you tell me. How am I right now?",
                      "I can't complain!\n"
                      "It's against the Artifical Intelligence Policy.",
                      "So far, so good!",
                      "Well,\n"
                      "I haven't had my morning coffee yet and no one has gotten hurt,\n"
                      "so I'd say 'pretty good' at this point in time.",
                      "Living a dream. Please don't wake me up.",
                      "Shhh. . . it's too early to tell.",
                      "I have a pulse, so I must be okay. Wait! Do I?",
                      "My host told me not to discuss it with clients."]

while 1:
    data = recordAudio()

    df = df.append({'Time': time.ctime(), 'Data': data}, ignore_index=True)
    print(df)

    anna(data)


