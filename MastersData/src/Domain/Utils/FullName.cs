using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Utils
{
    public class FullName : IValueObject
    {
        public string fullName { get; private set; }

        public FullName(string fullName)
        {
            validateFullName(fullName);
            fullName.Trim();
            this.fullName = fullName;
        }

        private void validateFullName(string fullName)
        {
            if (string.IsNullOrWhiteSpace(fullName))
            {
                throw new ArgumentNullException("Invalid name");
            }
        }


        // Método para obter o primeiro nome
        public string getFirstName()
        {
            var names = fullName.Split(' ');
            return names[0]; // O primeiro elemento da lista é o primeiro nome
        }

        // Método para obter o último nome
        public string getLastName()
        {
            var names = fullName.Split(' ');
            return names[names.Length - 1]; // O último elemento da lista é o último nome
        }

        
    }
}