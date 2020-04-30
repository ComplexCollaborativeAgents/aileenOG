import Tkinter
import Queue

queue = Queue.Queue()


class TextScrollCombo(Tkinter.Frame):
    def __init__(self, *args, **kwargs):
        Tkinter.Frame.__init__(self, *args, **kwargs)

        # ensure a consistent GUI size
        self.grid_propagate(False)
        # implement stretchability
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # create a Text widget
        self.txt = Tkinter.Text(self, state=Tkinter.DISABLED)
        self.txt.grid(row=0, column=0, sticky="nsew")
        self.txt.config(undo=True, wrap="word", font=("Ubuntu", 16))
        self.txt.tag_configure("signal", foreground="blue")
        self.txt.tag_configure("success", foreground="green")
        self.txt.tag_configure("failure", foreground="red")

        # create a Scrollbar and associate it with txt
        scrollb = Tkinter.Scrollbar(self, command=self.txt.yview)
        scrollb.grid(row=0, column=1, sticky="nsew")
        self.txt["yscrollcommand"] = scrollb.set

    def writeline(self, line):
        self.txt.config(state=Tkinter.NORMAL)
        self.txt.insert(Tkinter.END, line + "\n")
        self.txt.see(Tkinter.END)
        self.txt.config(state=Tkinter.DISABLED)
        self._highlight(line)

    def _highlight(self, line):
        word_color = {
            "inform": "signal",
            "verify": "signal",
            "success": "success",
            "failure": "failure"
        }
        for word, color in word_color.items():
            if word in line:
                column = int(self.txt.index('end-2c').split('.')[0])
                start = line.index(word)
                end = start + len(word)
                self.txt.tag_add(color, "{}.{}".format(column, start), "{}.{}".format(column, end))


def run():
    root = Tkinter.Tk()

    combo = TextScrollCombo(root)
    combo.pack(fill="both", expand=True)
    combo.config(width=600, height=600)

    def update():
        while not queue.empty():
            line = queue.get()
            combo.writeline(line)
        root.after(1, update)

    root.after(1, update)
    root.mainloop()


def log(line):
    queue.put(line)
