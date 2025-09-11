from psychopy import visual, core, event, gui
import csv, random, os

# ----------------- Experiment info dialog -----------------
exp_info = {'Participant': '', 'Session': '1'}
dlg = gui.DlgFromDict(dictionary=exp_info, title='Partial Report (Sperling)')
if not dlg.OK:
    core.quit()

filename = f"Partial_report_{exp_info['Participant']}_S{exp_info['Session']}.csv"
filepath = os.path.join(os.getcwd(), filename)

# ----------------- Window and stimuli -----------------
win = visual.Window(size=[1920, 1200], color='black', units='pix', fullscr=True)

letterArray = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

# Stimuli
matrix_text = visual.TextStim(win, text='', height=50, color='white', pos=(0, 0), alignText='center')
fixation = visual.TextStim(win, text='+', height=60, color='white', pos=(0, 0))
tone_marker_pos = {0: (0, 100), 1: (0, 0), 2: (0, -100)}  # top, middle, bottom row positions

# ----------------- Timing -----------------

fps = win.getActualFrameRate()
print(fps)
if fps is None or fps <= 0:
    fps = 60.0
frame_dur = 1.0 / fps

fixation_dur = 30 * frame_dur
stim_dur = 15 * frame_dur
isi_dur = 10 * frame_dur
tone_dur = 15 * frame_dur

# ----------------- Instructions -----------------
instr = visual.TextStim(win,
    text=(
          "You will briefly see a 3x4 grid of letters.\n"
          "After the display, an asterisk (*) will indicate which ROW you should recall.\n"
          "Type ALL FOUR letters from that row, in order, then press ENTER.\n\n"
          "Use BACKSPACE to correct mistakes.\n\n"
          "Press any key to begin."),
    height=30, color='white', wrapWidth=1600, pos=(0, 0))
instr.draw()
win.flip()
event.waitKeys()

# ----------------- Helper: present one trial -----------------
def present_trial(display_matrix):
    # Present fixation
    fixation.draw()
    win.flip()
    core.wait(fixation_dur)

    # Present matrix
    matrix_lines = []
    for r in range(3):
        matrix_lines.append(' '.join(display_matrix[r]))
    matrix_text.text = '\n'.join(matrix_lines)
    matrix_text.draw()
    win.flip()
    core.wait(stim_dur)

    # Clear display (ISI)
    matrix_text.text = ''
    win.flip()
    core.wait(isi_dur)

    # Choose tone row and show tone marker
    tone_row = random.randint(0, 2)
    tone = visual.TextStim(win, text='*', height=60, color='white', pos=tone_marker_pos[tone_row])
    tone.draw()
    win.flip()
    core.wait(tone_dur)

    # Ask for row response (all 4 letters)
    correct_row = ''.join(display_matrix[tone_row])
    typed = ''
    response = None
    prompt = visual.TextStim(win, height=28, color='white', pos=(0, -200), wrapWidth=1400)

    while True:
        prompt.text = (f"Type the 4 letters from the cued row (in order) and press ENTER:\n\n{typed}")
        prompt.draw()
        win.flip()
        keys = event.waitKeys()
        for k in keys:
            if k == 'escape':
                return 'escape', None, tone_row, correct_row
            if k == 'backspace':
                typed = typed[:-1]
            elif k in ['return', 'enter']:
                if len(typed) >= 1:
                    response = typed.upper()
                    return response, typed.upper(), tone_row, correct_row
            elif len(k) == 1:
                ch = k.upper()
                if ch.isalpha():
                    typed += ch
        # loop until ENTER

# ----------------- Main experiment loops -----------------
results = []
practice_trials = 5
n_trials = 15

# Practice
practice_instr = visual.TextStim(win, text="Practice trials: Press any key to continue", height=28, color='white')
practice_instr.draw()
win.flip()
event.waitKeys()

for t in range(practice_trials):
    displayMatrix = [random.sample(letterArray, 4) for _ in range(3)]
    resp, typed, tone_row, correct_row = present_trial(displayMatrix)
    if resp == 'escape':
        win.close()
        core.quit()
    results.append({
        'trial': t+1,
        'phase': 'practice',
        'matrix': '\\n'.join([' '.join(r) for r in displayMatrix]),
        'tone_row': tone_row,
        'correct_row': correct_row,
        'response_row': resp
    })

mainInstr = visual.TextStim(win, text="End of practice. Press any key to start the main experiment.", height=28, color='white')
mainInstr.draw()
win.flip()
event.waitKeys()

# Actual experiment
for t in range(n_trials):
    displayMatrix = [random.sample(letterArray, 4) for _ in range(3)]
    resp, typed, tone_row, correct_row = present_trial(displayMatrix)
    if resp == 'escape':
        break
    results.append({
        'trial': t+1,
        'phase': 'experiment',
        'matrix': '\\n'.join([' '.join(r) for r in displayMatrix]),
        'tone_row': tone_row,
        'correct_row': correct_row,
        'response_row': resp
    })

# ----------------- Save results -----------------
with open(filepath, mode='w', newline='') as file:
    fieldnames = ['trial', 'phase', 'matrix', 'tone_row', 'correct_row', 'response_row']
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()
    for row in results:
        writer.writerow(row)

# ----------------- Cleanup -----------------
thanks = visual.TextStim(win, text="Thank you. The experiment is finished.\n\nPress any key to exit.", height=28, color='white')
thanks.draw()
win.flip()
event.waitKeys()
win.close()
core.quit()
