import random
##Each section shows the words that will be recongnised and from which list it belongs to


verifications = ['are you sure','you sure','sure','Sure?']
random_verification = random.choice(verifications)

greetings = ['hello','welcome','hi','Hi','Hey!','hey','sup','yo','Hello','greetings','good day','good day']
random_greeting = random.choice(greetings)

approvals = ["that's good",'that is good','good']
random_approval = random.choice(approvals)

questions = ['How are you?','How are you doing?']
random_question = random.choice(questions)

responses = ['Okay',"I'm fine",'im fine','im good thank you']
random_response = random.choice(responses)

stucks = ['lets talk']
random_stuck = random.choice(stucks)

validations = ['yes','Yes','yeah','Yea','yea','no','No','Nah','nah','not this time','affirmative','true','beyond a doubt']
random_validation = random.choice(validations)

icebreakers = ['do you know what the strongest living thing in the wolrd is?']
random_icebreaker = random.choice(icebreakers)

answers = ['yes','ant']
random_answer = random.choice(answers)

while True:

    
##These are the requirements for each random input and output that is required
        user = input (">>>Me: ")
        if user in greetings:
            print(random_greeting,random_question)
##user inputs a greeting and the response is a greeting and a question
        elif user in questions:
            print(random_response)
            ##question and then a asnwer
        elif user in verifications:
            print(random_validation)
##verification and a validation
        elif user in responses:
            print(random_approval)
##response and a approval
        elif user in stucks:
            print(random_icebreaker)
            if user in answers:
                print(random_validation)    
        else:
            print("Sorry i dont understand. Use words like 'lets talk' or something like 'help'.")





