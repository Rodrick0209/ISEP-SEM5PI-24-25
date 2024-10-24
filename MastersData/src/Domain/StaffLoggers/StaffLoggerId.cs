using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.StaffLoggers
{
    public class StaffLoggerId : EntityId
    {
        [JsonConstructor]
        public StaffLoggerId(Guid value) : base(value) { }

        public StaffLoggerId(string value) : base(value) 
        {
            if (!IsValid(value))
            {
                throw new ArgumentException("Invalid Staff ID format.");
            }
        }

        // Construtor para criar StaffId a partir de uma string específica
        public static StaffLoggerId Create(string value)
        {
            return new StaffLoggerId(value);
        }

        override
        protected object createFromString(string text)
        {
            // Validação e criação de StaffId a partir da string
            if (!IsValid(text))
            {
                throw new ArgumentException("Invalid Staff ID format.");
            }
            return text; // Retorna o texto validado
        }

        override
        public string AsString()
        {
            return base.ObjValue.ToString();
        }

        public bool IsValid(string value)
        {
            // Validação básica para verificar se o ID segue o padrão esperado
            return System.Text.RegularExpressions.Regex.IsMatch(
                value, @"^(N|D|O)\d{4}\d{5}$");
        }

        public bool IsValid()
        {
            // Utiliza o método IsValid sobre o valor atual
            return IsValid(AsString());
        }
    }
}