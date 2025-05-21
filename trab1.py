from pyswip import Prolog

class PrologWrapper:
    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("trab-of.pl")

    def _consult(self, file):
        self.prolog.consult(file)

    def _query(self, query):
        return list(self.prolog.query(query))

    def _assert_fact(self, fact):
        self.query(f"assertz({fact})")
    
    def _format(self, entry):
        """
        Formata uma entrada:
        - Se for string, remove espaços e acentos e retorna em minúsculo.
        - Se for lista, aplica o mesmo formato em cada item.
        """
        if isinstance(entry, str):
            return entry.replace(" ", "_").replace("ç", "c").lower()
        elif isinstance(entry, list):
            return f"[{', '.join(map(self._format, entry))}]"
        else:
            return str(entry)

    def diagnostico(self, problema):
        """
        Retorna o diagnóstico baseado no sintoma fornecido.
        """
        resultado = self._query(f"diagnostico(Sintoma, {self._format(problema)})")
        if resultado:
            return resultado[0]['Sintoma']
        else:
            return None
    
    def solucao(self, problema : str):
        """
        Retorna a solução baseada no Problema fornecido.
        """
        resultado = self._query(f"solucao({self._format(problema)}, Tempo, Custo)")
        if resultado:
            return resultado[0]['Tempo'], resultado[0]['Custo']
        else:
            return None
    
    def nivelCriticidade(self, problema : str):
        """
        Retorna o nível de criticidade baseado no Problema fornecido.
        """
        resultado = self._query(f"nivel_criticidade({self._format(problema)}, Nivel)")
        if resultado:
            return resultado[0]['Nivel']
        else:
            return None
    
    def componentesAfetados(self, problema : str):
        """
        Retorna os componentes afetados baseado no Problema fornecido.
        """
        resultado = self._query(f"componentes_afetados({self._format(problema)}, Componentes)")
        if resultado:
            return resultado[0]['Componentes']
        else:
            return None
        
    def sistemaProblema(self, problema : str):
        """
        Retorna o sistema baseado no Problema fornecido.
        """
        resultado = self._query(f"sistema_problema({self._format(problema)}, Sistema)")
        if resultado:
            return resultado[0]['Sistema']
        else:
            return None
    
    def todosProblemas(self):
        """
        Retorna todos os problemas conhecidos.
        """
        resultado = self._query("todos_os_problemas(Problema)")
        if resultado:
            return [res['Problema'] for res in resultado]
        else:
            return []
    
    def possiveisProblemas(self, sintoma : str):
        """
        Retorna os possíveis problemas baseados no sintoma fornecido.
        """
        resultado = self._query(f"possiveis_problemas({self._format(sintoma)}, Problemas)")
        if resultado:
            return [res['Problemas'] for res in resultado]
        else:
            return []
    
    def todosSintomasSistema(self, sistema : str):
        """
        Retorna todos os sintomas do sistema fornecido.
        """
        resultado = self._query(f"sintomas_sistema({self._format(sistema)}, Sintomas)")
        if resultado:
            return [res['Sintomas'] for res in resultado]
        else:
            return []
    
    def ordenarProblemasGravidade(self, problemas):
        """
        Ordena os problemas por gravidade.
        """
        resultado = self._query(f"ordenar_problemas_por_gravidade({self._format(problemas)}, Ordenados)")
        if resultado:
            return [res['Ordenados'] for res in resultado]
        else:
            return []
    
    def maxProblemasResolvidosATempo(self, problema, tempomax : float):
        """
        Verifica se o problema foi resolvido a tempo.
        """
        resultado = self._query(f"problemas_resolvidos_a_tempo({self._format(problema)},{tempomax}, Resolvido)")
        if resultado:
            return [res['Resolvido'] for res in resultado]
        else:
            return None
    
    def todosSintomas(self):
        """
        Retorna todos os sintomas conhecidos.
        """
        resultado = self._query("todos_os_sintomas(Sintoma)")
        if resultado:
            return [res['Sintoma'] for res in resultado]
        else:
            return []
    
    def todosProblemas(self):
        """
        Retorna todos os problemas conhecidos.
        """
        resultado = self._query("todos_os_problemas(Problema)")
        if resultado:
            return [res['Problema'] for res in resultado]
        else:
            return []
    
    def todosComponentes(self):
        """
        Retorna todos os componentes conhecidos.
        """
        resultado = self._query("todos_os_componentes(Componentes)")
        if resultado:
            return [res['Componentes'] for res in resultado]
        else:
            return []
        
    
if __name__ == "__main__":
    p = PrologWrapper()
    print(p.diagnostico("bomba_hidraulica_com_defeito"))
    print(p.solucao("bomba_hidraulica_com_defeito"))
    print(p.nivelCriticidade("bomba_hidraulica_com_defeito"))
    print(p.componentesAfetados("bomba_hidraulica_com_defeito"))
    print(p.sistemaProblema("bomba_hidraulica_com_defeito"))
    print(p.todosProblemas())
    print(p.possiveisProblemas("dificuldade_partida"))
    print(p.todosSintomasSistema("sistema_arrefecimento"))
    print(p.ordenarProblemasGravidade(["bomba hidraulica Com_defeito", "bateria_descarregando", "alternador_sem_carga","radiador_obstruido"]))
    print(p.maxProblemasResolvidosATempo(['bomba_hidraulica_com_defeito', 'alternador_sem_carga', 'radiador_obstruido', 'bateria_descarregando', 'embreagem_desgastada', 'caixa_direcao_folgada', 'semieixo_danificado'], 10))