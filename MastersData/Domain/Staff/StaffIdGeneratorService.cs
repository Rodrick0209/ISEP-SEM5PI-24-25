using System;
using System.Collections.Generic;
using DDDSample1.Domain.Staff;
using Domain.StaffMembers;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffIdGeneratorService
    {
        private readonly Dictionary<Category, int> _sequentialNumbers;

        public StaffIdGeneratorService()
        {
            // Inicializa o dicionário com contadores separados por categoria
            _sequentialNumbers = new Dictionary<Category, int>
            {
                { Category.Doctor, 0 },           // Contador para médicos e internos
                { Category.InternDoctor, 0 },      // Usará o mesmo contador que o médico
                { Category.Nurse, 0 },
                { Category.Technician, 0 }
            };
        }

        public StaffId GenerateStaffId(Category category, DateTime recruitmentDate)
        {
            // Determina o prefixo com base na categoria
            string prefix = category switch
            {
                Category.Doctor => "D",
                Category.InternDoctor => "D", // InternDoctor também usa "D"
                Category.Nurse => "N",
                _ => "O" // Outros (Technician ou qualquer outra categoria)
            };

            // Extrai o ano de recrutamento
            string year = recruitmentDate.Year.ToString();

            // Incrementa o número sequencial para a categoria correspondente
            int sequentialNumber = _sequentialNumbers[category]++;
            string sequentialNumberString = sequentialNumber.ToString("D5"); // Formato de 5 dígitos

            /// Concatena prefixo, ano e número sequencial
            string staffIdValue = $"{prefix}{year}{sequentialNumberString}";

            // Retorna um novo objeto StaffId
            return new StaffId(staffIdValue);
        }
    }
}
