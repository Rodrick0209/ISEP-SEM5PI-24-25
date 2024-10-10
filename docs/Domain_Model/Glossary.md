# Glossário

## Utilizador
Representa qualquer pessoa que interage com o sistema, seja ele um administrador, um profissional de saúde ou um paciente.

### Atributos:
- Nome de Utilizador
- Função (ex: Administrador, Médico, Enfermeiro, Técnico, Paciente)
- Email

### Regras:
- Utilizadores de backoffice são registados pelo administrador através de um processo externo.
- Pacientes podem se auto registar.
- O registo do utilizador no sistema de gestão de identidade e acessos (IAM) está ligado ao seu registo no backoffice.
- Todos os utilizadores autenticam-se usando o IAM.

## Paciente
Indivíduos que recebem cuidados médicos.

### Atributos:
- Primeiro Nome
- Último Nome
- Nome Completo
- Data de Nascimento
- Género
- Número do Processo Médico (identificador único)
- Informações de Contacto (email, telefone)
- Alergias/Condições Médicas (opcional)
- Contacto de Emergência
- Histórico de Consultas (lista de consultas anteriores e futuras)

### Regras:
- Um paciente deve ser único em termos de Número do Processo Médico, Email e Telefone.
- Dados sensíveis (como histórico médico) devem estar em conformidade com o GDPR, permitindo aos pacientes controlar o acesso aos seus dados.

## Profissional de Saúde
Profissionais que prestam cuidados de saúde.

### Atributos:
- Primeiro Nome
- Último Nome
- Nome Completo
- Número da Licença (identificador único)
- Especialização (ex: Cardiologia, Ortopedia). Cada profissional de saúde tem apenas uma especialidade.
- Informações de Contacto (email, telefone)
- Intervalos de Disponibilidade (a lista de intervalos de tempo que o profissional define como estando disponível para consultas)

### Regras:
- Um profissional de saúde deve ser único em termos de Número da Licença, Email e Telefone.
- Os profissionais de saúde definem os seus intervalos de disponibilidade, ex: intervalo 1: 2024-09-25:14h00-18h00; intervalo 2: 2024-09-25:19h00/2024-09-26:02h00.
- Os intervalos de disponibilidade permanecem inalterados quando os intervalos são usados para uma consulta.
- Os profissionais de saúde podem gerir várias consultas, mas não podem ser marcados duas vezes ao mesmo tempo.

## Pedido de Cirurgia
A cirurgia que é solicitada para posterior agendamento.

### Atributos:
- ID (identificador único)
- ID do Paciente (ligado a um paciente específico)
- ID do Médico (ligado a um médico que o solicita)
- ID do Tipo de Cirurgia (o tipo de cirurgia a ser realizada no paciente)
- Data Limite (a data limite sugerida para realizar a cirurgia)
- Prioridade (a prioridade para realizar a cirurgia)

## Tipo de Cirurgia
Representa tipos predefinidos de cirurgias ou procedimentos médicos.

### Atributos:
- ID (identificador único)
- Nome (ex: Apendicectomia, Bypass Cardíaco)
- Pessoal Necessário por Especialização (lista de pessoal essencial em relação à especialização)
- Duração Estimada (do tipo de cirurgia)

## Consulta
Representa a cirurgia agendada de um paciente por um conjunto de profissionais de saúde, ocorrendo numa sala num determinado intervalo de tempo.

### Atributos:
- ID (identificador único)
- ID do Pedido (ligado ao pedido que deu origem a esta consulta)
- ID da Sala (ligado a uma sala específica)
- Data e Hora (da cirurgia)
- Estado (agendado, concluído, cancelado)

### Regras:
- As consultas de cirurgias devem ser atribuídas a um conjunto de profissionais de saúde, a uma sala num determinado intervalo de tempo.
- As cirurgias não podem exceder o tempo estimado, a menos que sejam remarcadas.
- Uma consulta não pode ser agendada se o profissional de saúde ou a sala não estiver disponível nesse horário.
- O tipo de consulta deve corresponder às especializações dos profissionais de saúde e à disponibilidade da sala.

## Sala de Cirurgia
Representa salas de cirurgia na unidade de saúde para a realização de cirurgias.

### Atributos:
- Número da Sala (identificador único)
- Tipo (ex: Sala de operações, Sala de consultas, UTI)
- Capacidade (número máximo de pacientes ou profissionais de saúde)
- Equipamento Atribuído (lista de equipamentos na sala)
- Estado Atual (disponível, ocupada, em manutenção)
- Intervalos de Manutenção (os intervalos definidos para a manutenção da sala)

### Regras:
- Cada sala pode acolher apenas um evento de cada vez (seja uma consulta, cirurgia ou reunião).
- O agendamento da sala deve ser gerido com base na disponibilidade do médico e do equipamento.
- Assume-se que todas as salas estão totalmente equipadas com o equipamento necessário para cada tipo de cirurgia.
