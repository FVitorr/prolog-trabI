import customtkinter as ctk
from tkinter import *
from trab1 import PrologWrapper

ctk.set_appearance_mode("dark")
ctk.set_default_color_theme("dark-blue")

prolog = PrologWrapper()

app = ctk.CTk()
app.title("Sistema Especialista")
app.geometry("1600x900")

regras = [
    'Diagnóstico',
    'Solução',
    'Nivel Criticidade',
    'Componentes afetados',
    'Sistema problemático',
    'Todos os problemas',
    'Possíveis problemas',
    'Sintomas de um sistema',
    'Problemas por gravidade',
    'Problemas resolvidos a tempo'
]

sintomas = [f'Sintoma {i}' for i in range(1, 11)]
problemas = [f'Problema {i}' for i in range(1, 11)]
sistemas = [f'Sistema {i}' for i in range(1, 11)]

tabview = ctk.CTkTabview(app)
tabview.pack(pady=20, padx=20, fill="both", expand=True)

for regra in regras:
    tabview.add(regra)
    tabview.tab(regra).grid_columnconfigure(0, weight=1)
    tabview.tab(regra).grid_rowconfigure(0, weight=1)

# ========== ABA DIAGNÓSTICO ==========
diagnostico_frame = ctk.CTkFrame(tabview.tab('Diagnóstico'))
ctk.CTkLabel(diagnostico_frame, text="Selecione os sintomas:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)

checkboxes_diagnostico = []
for sintoma in sintomas:
    checkbox = ctk.CTkCheckBox(diagnostico_frame, text=sintoma)
    checkbox.pack(pady=2, padx=10)
    checkboxes_diagnostico.append(checkbox)

buttonDiagnostico = ctk.CTkButton(diagnostico_frame, text='Diagnosticar', command=lambda: print("Diagnóstico realizado"))
buttonDiagnostico.pack(pady=10)

ctk.CTkLabel(diagnostico_frame, text="Resultados:", font=ctk.CTkFont(size=14, weight="bold")).pack(pady=(20, 5))
resultados_frame = ctk.CTkFrame(diagnostico_frame)
resultados_frame.pack(pady=5, padx=10, fill="both", expand=True)

diagnostico_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA SOLUÇÃO ==========
solucao_frame = ctk.CTkFrame(tabview.tab('Solução'))
ctk.CTkLabel(solucao_frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
optionmenu_solucao = ctk.CTkOptionMenu(solucao_frame, values=problemas)
optionmenu_solucao.pack(pady=5, padx=10)
ctk.CTkButton(solucao_frame, text="Ver Solução", command=lambda: print("Solução exibida")).pack(pady=10)
solucao_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA NÍVEL CRITICIDADE ==========
nivel_criticidade_frame = ctk.CTkFrame(tabview.tab('Nivel Criticidade'))
ctk.CTkLabel(nivel_criticidade_frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
optionmenu_nc = ctk.CTkOptionMenu(nivel_criticidade_frame, values=problemas)
optionmenu_nc.pack(pady=5, padx=10)
ctk.CTkButton(nivel_criticidade_frame, text="Ver Nível de Criticidade").pack(pady=10)
nivel_criticidade_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA COMPONENTES AFETADOS ==========
componentes_afetados_frame = ctk.CTkFrame(tabview.tab('Componentes afetados'))
ctk.CTkLabel(componentes_afetados_frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
optionmenu_ca = ctk.CTkOptionMenu(componentes_afetados_frame, values=problemas)
optionmenu_ca.pack(pady=5, padx=10)
ctk.CTkButton(componentes_afetados_frame, text="Ver Componentes Afetados").pack(pady=10)
componentes_afetados_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA SISTEMA PROBLEMÁTICO ==========
sistema_problematico_frame = ctk.CTkFrame(tabview.tab('Sistema problemático'))
ctk.CTkLabel(sistema_problematico_frame, text="Escolha um problema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
optionmenu_sp = ctk.CTkOptionMenu(sistema_problematico_frame, values=problemas)
optionmenu_sp.pack(pady=5, padx=10)
ctk.CTkButton(sistema_problematico_frame, text="Ver Sistema Relacionado").pack(pady=10)
sistema_problematico_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA TODOS OS PROBLEMAS ==========
todos_problemas_frame = ctk.CTkFrame(tabview.tab('Todos os problemas'))
ctk.CTkLabel(todos_problemas_frame, text="Listar todos os problemas conhecidos:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
ctk.CTkButton(todos_problemas_frame, text="Exibir Problemas", command=lambda: print(problemas)).pack(pady=10)
todos_problemas_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA POSSÍVEIS PROBLEMAS ==========
possiveis_problemas_frame = ctk.CTkFrame(tabview.tab('Possíveis problemas'))
ctk.CTkLabel(possiveis_problemas_frame, text="Escolha um sintoma:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
optionmenu_pp = ctk.CTkOptionMenu(possiveis_problemas_frame, values=sintomas)
optionmenu_pp.pack(pady=5, padx=10)
ctk.CTkButton(possiveis_problemas_frame, text="Buscar Problemas").pack(pady=10)
possiveis_problemas_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA SINTOMAS DE UM SISTEMA ==========
sintomas_sistema_frame = ctk.CTkFrame(tabview.tab('Sintomas de um sistema'))
ctk.CTkLabel(sintomas_sistema_frame, text="Escolha um sistema:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
optionmenu_ss = ctk.CTkOptionMenu(sintomas_sistema_frame, values=sistemas)
optionmenu_ss.pack(pady=5, padx=10)
ctk.CTkButton(sintomas_sistema_frame, text="Ver Sintomas do Sistema").pack(pady=10)
sintomas_sistema_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA PROBLEMAS POR GRAVIDADE ==========
gravidade_frame = ctk.CTkFrame(tabview.tab('Problemas por gravidade'))
ctk.CTkLabel(gravidade_frame, text="Ver problemas por nível de gravidade:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)
ctk.CTkButton(gravidade_frame, text="Exibir Problemas Críticos").pack(pady=10)
gravidade_frame.pack(pady=20, padx=20, fill="both", expand=True)

# ========== ABA PROBLEMAS RESOLVIDOS A TEMPO ==========
problemas_resolvidos_frame = ctk.CTkFrame(tabview.tab('Problemas resolvidos a tempo'))
ctk.CTkLabel(problemas_resolvidos_frame, text="Selecione os problemas resolvidos:", font=ctk.CTkFont(size=16, weight="bold")).pack(pady=10)

checkboxes_resolvidos = []
for problema in problemas:
    checkbox = ctk.CTkCheckBox(problemas_resolvidos_frame, text=problema)
    checkbox.pack(pady=2, padx=10)
    checkboxes_resolvidos.append(checkbox)

entry = ctk.CTkEntry(problemas_resolvidos_frame, placeholder_text="Digite o tempo (em horas)")
entry.pack(pady=10, padx=10)

ctk.CTkButton(problemas_resolvidos_frame, text="Verificar Problemas no Prazo").pack(pady=10)

problemas_resolvidos_frame.pack(pady=20, padx=20, fill="both", expand=True)

app.mainloop()
