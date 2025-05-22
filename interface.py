import customtkinter as ctk
from tkinter import *
from trab1 import PrologWrapper

class SistemaEspecialistaApp(ctk.CTk):
    def __init__(self):
        super().__init__()
        ctk.set_appearance_mode("dark")
        ctk.set_default_color_theme("dark-blue")

        self.title("Sistema Especialista")
        self.geometry("1600x900")

        self.prolog = PrologWrapper()

        self.regras = [
            'Diagnóstico', 'Solução', 'Nivel Criticidade', 'Componentes afetados',
            'Sistema problemático', 'Todos os problemas','Sintomas de um sistema', 
            'Problemas por gravidade', 'Problemas resolvidos a tempo'
        ]

        self.sintomas = [i.replace("_", " ").title() for i in self.prolog.todosSintomas()[0]]
        self.problemas = [i.replace("_", " ").title() for i in self.prolog.todosProblemasDropDown()[0]]
        self.sistemas = [i.replace("_", " ").title() for i in self.prolog.todosSistemasDropdown()[0]]

        self.resultados_solucao = None

        self.build_tabs()

    def build_tabs(self):
        self.tabview = ctk.CTkTabview(self)
        self.tabview.pack(pady=20, padx=20, fill="both", expand=True)

        self.frames = {}

        for regra in self.regras:
            self.tabview.add(regra)
            self.tabview.tab(regra).grid_columnconfigure(0, weight=1)
            self.tabview.tab(regra).grid_rowconfigure(0, weight=1)
            self.frames[regra] = ctk.CTkFrame(self.tabview.tab(regra))
            self.frames[regra].pack(pady=20, padx=20, fill="both", expand=True)

        self.aba_diagnostico()
        self.aba_solucao()
        self.aba_nivel_criticidade()
        self.aba_componentes_afetados()
        self.aba_sistema_problematico()
        self.aba_todos_problemas()
        self.aba_sintomas_sistema()
        self.aba_problemas_gravidade()
        self.aba_problemas_resolvidos()

    def aba_diagnostico(self):
        frame = self.frames['Diagnóstico']
        ctk.CTkLabel(frame, text="Selecione os sintomas:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)

        self.checkboxes_diagnostico = []
        checkbox_frame = ctk.CTkFrame(frame)
        checkbox_frame.pack(pady=10, padx=10, fill="both", expand=True)

        for idx, sintoma in enumerate(self.sintomas):
            linha, coluna = divmod(idx, 7)
            cb = ctk.CTkCheckBox(checkbox_frame, text=sintoma)
            cb.grid(row=linha, column=coluna, padx=10, pady=5, sticky="w")
            self.checkboxes_diagnostico.append(cb)

        ctk.CTkButton(frame, text='Diagnosticar',
                      command=self.rodar_diagnostico).pack(pady=10)
        ctk.CTkButton(frame, text='Limpar', command=self.limpar_diagnostico).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_diagnostico = ctk.CTkFrame(frame)
        self.resultados_diagnostico.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_diagnostico(self):
        selecionados = [cb.get() for cb in self.checkboxes_diagnostico]
        sintomas_selecionados = [self.sintomas[i].replace(" ", "_").lower() for i, val in enumerate(selecionados) if val]
        
        for widget in self.resultados_diagnostico.winfo_children():
            widget.destroy()
            
        if not sintomas_selecionados:
            ctk.CTkLabel(self.resultados_diagnostico, text="Selecione pelo menos um sintoma", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            

        problemas = self.prolog.diagnostico(sintomas_selecionados)

        
        if problemas:
            problemas_unicos = list(set(problemas))
            for problema in problemas_unicos:
                ctk.CTkLabel(self.resultados_diagnostico, text=problema, 
                            font=ctk.CTkFont(size=14)).pack(pady=5)
        else:
            ctk.CTkLabel(self.resultados_diagnostico, text="Nenhum problema encontrado", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_diagnostico(self):
        for cb in self.checkboxes_diagnostico:
            cb.deselect()
        for widget in self.resultados_diagnostico.winfo_children():
            widget.destroy()

    def aba_solucao(self):
        frame = self.frames['Solução']
        ctk.CTkLabel(frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.optionmenu_solucao = ctk.CTkOptionMenu(frame, values=self.problemas)
        self.optionmenu_solucao.pack(pady=5, padx=10)
        
        ctk.CTkButton(frame, text="Ver Solução", command=self.rodar_solucao).pack(pady=10)
        ctk.CTkButton(frame, text="Limpar", command=self.limpar_solucao).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_solucao = ctk.CTkFrame(frame)
        self.resultados_solucao.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_solucao(self):
        problema = self.optionmenu_solucao.get().replace(" ", "_").lower()
        
        for widget in self.resultados_solucao.winfo_children():
            widget.destroy()
            
        if not problema:
            ctk.CTkLabel(self.resultados_solucao, text="Selecione um problema", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        resultado = self.prolog.solucao(problema)
        if resultado:
            tempo, custo = resultado
            ctk.CTkLabel(self.resultados_solucao, 
                        text=f"Tempo estimado: {tempo} horas\nCusto estimado: R${custo}", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)
        else:
            ctk.CTkLabel(self.resultados_solucao, text="Solução não encontrada", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_solucao(self):
        self.optionmenu_solucao.set("")
        for widget in self.resultados_solucao.winfo_children():
            widget.destroy()

    def aba_nivel_criticidade(self):
        frame = self.frames['Nivel Criticidade']
        ctk.CTkLabel(frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.optionmenu_criticidade = ctk.CTkOptionMenu(frame, values=self.problemas)
        self.optionmenu_criticidade.pack(pady=5, padx=10)
        
        ctk.CTkButton(frame, text="Ver Nível de Criticidade", 
                     command=self.rodar_criticidade).pack(pady=10)
        ctk.CTkButton(frame, text="Limpar", command=self.limpar_criticidade).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_criticidade = ctk.CTkFrame(frame)
        self.resultados_criticidade.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_criticidade(self):
        problema = self.optionmenu_criticidade.get().replace(" ", "_").lower()
        
        for widget in self.resultados_criticidade.winfo_children():
            widget.destroy()
            
        if not problema:
            ctk.CTkLabel(self.resultados_criticidade, text="Selecione um problema", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        resultado = self.prolog.nivelCriticidade(problema)
        if resultado:
            ctk.CTkLabel(self.resultados_criticidade, 
                        text=f"Nível de criticidade: {resultado}", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)
        else:
            ctk.CTkLabel(self.resultados_criticidade, text="Nível não encontrado", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_criticidade(self):
        self.optionmenu_criticidade.set("")
        for widget in self.resultados_criticidade.winfo_children():
            widget.destroy()

    def aba_componentes_afetados(self):
        frame = self.frames['Componentes afetados']
        ctk.CTkLabel(frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.optionmenu_componentes = ctk.CTkOptionMenu(frame, values=self.problemas)
        self.optionmenu_componentes.pack(pady=5, padx=10)
        
        ctk.CTkButton(frame, text="Ver Componentes Afetados", 
                     command=self.rodar_componentes).pack(pady=10)
        ctk.CTkButton(frame, text="Limpar", command=self.limpar_componentes).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_componentes = ctk.CTkFrame(frame)
        self.resultados_componentes.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_componentes(self):
        problema = self.optionmenu_componentes.get().replace(" ", "_").lower()
        
        for widget in self.resultados_componentes.winfo_children():
            widget.destroy()
            
        if not problema:
            ctk.CTkLabel(self.resultados_componentes, text="Selecione um problema", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        resultado = self.prolog.componentesAfetados(problema)
        if resultado:
            componentes = [comp.replace("_", " ").title() for comp in resultado]
            ctk.CTkLabel(self.resultados_componentes, 
                        text="Componentes afetados:\n" + "\n".join(componentes), 
                        font=ctk.CTkFont(size=14)).pack(pady=5)
        else:
            ctk.CTkLabel(self.resultados_componentes, text="Componentes não encontrados", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_componentes(self):
        self.optionmenu_componentes.set("")
        for widget in self.resultados_componentes.winfo_children():
            widget.destroy()

    def aba_sistema_problematico(self):
        frame = self.frames['Sistema problemático']
        ctk.CTkLabel(frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.optionmenu_sistema = ctk.CTkOptionMenu(frame, values=self.problemas)
        self.optionmenu_sistema.pack(pady=5, padx=10)
        
        ctk.CTkButton(frame, text="Ver Sistema Problemático", 
                     command=self.rodar_sistema).pack(pady=10)
        ctk.CTkButton(frame, text="Limpar", command=self.limpar_sistema).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_sistema = ctk.CTkFrame(frame)
        self.resultados_sistema.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_sistema(self):
        problema = self.optionmenu_sistema.get().replace(" ", "_").lower()
        
        for widget in self.resultados_sistema.winfo_children():
            widget.destroy()
            
        if not problema:
            ctk.CTkLabel(self.resultados_sistema, text="Selecione um problema", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        resultado = self.prolog.sistemaProblema(problema)
        if resultado:
            sistema = resultado.replace("_", " ").title()
            ctk.CTkLabel(self.resultados_sistema, 
                        text=f"Sistema problemático: {sistema}", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)
        else:
            ctk.CTkLabel(self.resultados_sistema, text="Sistema não encontrado", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_sistema(self):
        self.optionmenu_sistema.set("")
        for widget in self.resultados_sistema.winfo_children():
            widget.destroy()

    def aba_todos_problemas(self):
        frame = self.frames['Todos os problemas']
        ctk.CTkLabel(frame, text="Todos os problemas conhecidos:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.btn_todos_problemas = ctk.CTkButton(frame, text="Exibir Problemas", 
                                               command=self.rodar_todos_problemas)
        self.btn_todos_problemas.pack(pady=10)
        
        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_todos_problemas = ctk.CTkScrollableFrame(frame)
        self.resultados_todos_problemas.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_todos_problemas(self):
        for widget in self.resultados_todos_problemas.winfo_children():
            widget.destroy()
            
        problemas = self.prolog.todosProblemas()
        if problemas:
            problemas_formatados = [prob.replace("_", " ").title() for prob in problemas]
            for problema in problemas_formatados:
                ctk.CTkLabel(self.resultados_todos_problemas, text=problema, 
                            font=ctk.CTkFont(size=14)).pack(pady=2, anchor="w")
        else:
            ctk.CTkLabel(self.resultados_todos_problemas, text="Nenhum problema encontrado", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)


    def aba_sintomas_sistema(self):
        frame = self.frames['Sintomas de um sistema']
        ctk.CTkLabel(frame, text="Escolha um sistema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.optionmenu_sistema_sintomas = ctk.CTkOptionMenu(frame, values=self.sistemas)
        self.optionmenu_sistema_sintomas.pack(pady=5, padx=10)
        
        ctk.CTkButton(frame, text="Ver Sintomas", 
                     command=self.rodar_sintomas_sistema).pack(pady=10)
        ctk.CTkButton(frame, text="Limpar", command=self.limpar_sintomas_sistema).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_sintomas_sistema = ctk.CTkScrollableFrame(frame)
        self.resultados_sintomas_sistema.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_sintomas_sistema(self):
        sistema = self.optionmenu_sistema_sintomas.get().replace(" ", "_").lower()
        
        for widget in self.resultados_sintomas_sistema.winfo_children():
            widget.destroy()
            
        if not sistema:
            ctk.CTkLabel(self.resultados_sintomas_sistema, text="Selecione um sistema", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        sintomas = self.prolog.todosSintomasSistema(sistema)

        if sintomas:
            sintomas_formatados = [sint.replace("_", " ").title() for sint in sintomas]
            for sintoma in sintomas_formatados:
                ctk.CTkLabel(self.resultados_sintomas_sistema, text=sintoma, 
                            font=ctk.CTkFont(size=14)).pack(pady=2, anchor="w")
        else:
            ctk.CTkLabel(self.resultados_sintomas_sistema, text="Nenhum sintoma encontrado", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_sintomas_sistema(self):
        self.optionmenu_sistema_sintomas.set("")
        for widget in self.resultados_sintomas_sistema.winfo_children():
            widget.destroy()


    def aba_problemas_gravidade(self):
        frame = self.frames['Problemas por gravidade']
        ctk.CTkLabel(frame, text="Problemas ordenados por gravidade:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
        
        self.btn_problemas_gravidade = ctk.CTkButton(frame, text="Ordenar Problemas", 
                                                    command=self.rodar_problemas_gravidade)
        self.btn_problemas_gravidade.pack(pady=10)
        
        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_problemas_gravidade = ctk.CTkScrollableFrame(frame)
        self.resultados_problemas_gravidade.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_problemas_gravidade(self):
        for widget in self.resultados_problemas_gravidade.winfo_children():
            widget.destroy()
            
        problemas = self.prolog.todosProblemas()
        if problemas:
            problemas_ordenados = self.prolog.ordenarProblemasGravidade(problemas)
            if problemas_ordenados and problemas_ordenados[0]:
                for problema in problemas_ordenados[0]:
                    prob_formatado = problema.replace("_", " ").title()
                    ctk.CTkLabel(self.resultados_problemas_gravidade, text=prob_formatado, 
                                font=ctk.CTkFont(size=14)).pack(pady=2, anchor="w")
            else:
                ctk.CTkLabel(self.resultados_problemas_gravidade, 
                            text="Não foi possível ordenar os problemas", 
                            font=ctk.CTkFont(size=14)).pack(pady=5)
        else:
            ctk.CTkLabel(self.resultados_problemas_gravidade, text="Nenhum problema encontrado", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)


    def aba_problemas_resolvidos(self):
        frame = self.frames['Problemas resolvidos a tempo']
        ctk.CTkLabel(frame, text="Selecione os problemas:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)

        self.checkboxes_problemas = []
        checkbox_frame = ctk.CTkFrame(frame)
        checkbox_frame.pack(pady=10, padx=10, fill="both", expand=True)

        for idx, problema in enumerate(self.problemas):
            linha, coluna = divmod(idx, 7)
            cb = ctk.CTkCheckBox(checkbox_frame, text=problema)
            cb.grid(row=linha, column=coluna, padx=10, pady=5, sticky="w")
            self.checkboxes_problemas.append(cb)

        ctk.CTkLabel(frame, text="Tempo máximo disponível (horas):", font=ctk.CTkFont(size=14)).pack(pady=5)
        self.entry_tempo_max = ctk.CTkEntry(frame)
        self.entry_tempo_max.pack(pady=5, padx=10)
        
        ctk.CTkButton(frame, text="Verificar Problemas no Prazo", 
                     command=self.rodar_problemas_resolvidos).pack(pady=10)
        ctk.CTkButton(frame, text="Limpar", command=self.limpar_problemas_resolvidos).pack(pady=5)

        ctk.CTkLabel(frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
        self.resultados_problemas_resolvidos = ctk.CTkScrollableFrame(frame)
        self.resultados_problemas_resolvidos.pack(pady=5, padx=10, fill="both", expand=True)

    def rodar_problemas_resolvidos(self):
        selecionados = [cb.get() for cb in self.checkboxes_problemas]
        problemas_selecionados = [self.problemas[i].replace(" ", "_").lower() for i, val in enumerate(selecionados) if val]
        tempo_max = self.entry_tempo_max.get()
        
        for widget in self.resultados_problemas_resolvidos.winfo_children():
            widget.destroy()
            
        if not problemas_selecionados:
            ctk.CTkLabel(self.resultados_problemas_resolvidos, text="Selecione pelo menos um problema", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        if not tempo_max or not tempo_max.isdigit():
            ctk.CTkLabel(self.resultados_problemas_resolvidos, text="Digite um tempo válido", 
                         font=ctk.CTkFont(size=14)).pack(pady=5)
            return
            
        tempo_max = float(tempo_max)
        resolvidos = self.prolog.maxProblemasResolvidosATempo(problemas_selecionados, tempo_max)
        
        if resolvidos and resolvidos[0]:
            problemas_resolvidos = [prob.replace("_", " ").title() for prob in resolvidos[0]]
            ctk.CTkLabel(self.resultados_problemas_resolvidos, 
                        text="Problemas que podem ser resolvidos no tempo especificado:",
                        font=ctk.CTkFont(size=14, weight="bold")).pack(pady=5)
            
            for problema in problemas_resolvidos:
                ctk.CTkLabel(self.resultados_problemas_resolvidos, text=problema, 
                            font=ctk.CTkFont(size=14)).pack(pady=2, anchor="w")
        else:
            ctk.CTkLabel(self.resultados_problemas_resolvidos, 
                        text="Nenhum problema pode ser resolvido no tempo especificado", 
                        font=ctk.CTkFont(size=14)).pack(pady=5)

    def limpar_problemas_resolvidos(self):
        for cb in self.checkboxes_problemas:
            cb.deselect()
        self.entry_tempo_max.delete(0, 'end')
        for widget in self.resultados_problemas_resolvidos.winfo_children():
            widget.destroy()

if __name__ == "__main__":
    app = SistemaEspecialistaApp()
    app.mainloop()