def encontrar_melhor_bloqueador(defensores, poder_atk):
    # ordenar os defensores por menor resistência
    defensores_ordenados = sorted(defensores, key=lambda d: d["resistencia"])

    # tentar encontrar o melhor bloqueador, aquele que 




def encontrar_dois_bloqueadores(defensores):
    if len(defensores) < 2:
        return None, None, defensores

    defensores_ordenados = sorted(defensores, key=lambda d: d["resistencia"], reverse=True)
    b1, b2 = defensores_ordenados[:2]
    novos_defensores = [d for d in defensores if d != b1 and d != b2]
    return b1, b2, novos_defensores


def processar_atacantes(atacantes, defensores):
    bloqueios = []

    for atacante in atacantes:
        nome = atacante["nome"]
        poder_atk = atacante["poder"]
        habilidades = atacante.get("habilidades", [])
        bloqueadores = []
        dano = 0

        if "ameacar" in habilidades:
            b1, b2, defensores = encontrar_dois_bloqueadores(defensores)
            if b1 and b2:
                bloqueadores = [b1, b2]

        elif "atropelar" in habilidades:
            if defensores:
                bloqueador, defensores = encontrar_melhor_bloqueador(defensores, poder_atk)
                bloqueadores = [bloqueador]
                dano = max(0, poder_atk - bloqueador["resistencia"])
            else:
                dano = poder_atk

        elif "voar" in habilidades:
            defensores_voadores = [d for d in defensores if "voar" in d.get("habilidades", [])]
            defensores_alcance = [d for d in defensores if "alcance" in d.get("habilidades", [])]
            defensoresAptos = defensores_voadores + defensores_alcance
            if defensoresAptos:
                bloqueador, defensoresAptos = encontrar_melhor_bloqueador(defensoresAptos, poder_atk)
                bloqueadores = [bloqueador]
                defensores = [d for d in defensores if d != bloqueador]
            else:
                dano = poder_atk

        else:
            if defensores:
                bloqueador, defensores = encontrar_melhor_bloqueador(defensores, poder_atk)
                bloqueadores = [bloqueador]
            else:
                dano = poder_atk

        bloqueios.append({
            "atacante": nome,
            "bloqueadores": [b["nome"] for b in bloqueadores if b],
            "dano": dano
        })

    return bloqueios


# EXEMPLO DE USO
if __name__ == "__main__":
    atacantes = [
        {"nome": "Dragão", "poder": 5, "resistencia": 5, "habilidades": ["voar"]},
        {"nome": "Orc", "poder": 3, "resistencia": 3, "habilidades": ["ameacar"]},
        {"nome": "Zumbi", "poder": 2, "resistencia": 2, "habilidades": []}
    ]

    defensores = [
        {"nome": "Anjo", "poder": 2, "resistencia": 5, "habilidades": ["voar"]},
        {"nome": "Guerreiro", "poder": 3, "resistencia": 4, "habilidades": []},
        {"nome": "Clérigo", "poder": 1, "resistencia": 2, "habilidades": []}
    ]

    resultado = processar_atacantes(atacantes, defensores)

    print("\nResultado da fase de bloqueios:")
    for r in resultado:
        print(f"- {r['atacante']} foi bloqueado por {r['bloqueadores'] or 'ninguém'} e causou {r['dano']} de dano.")
