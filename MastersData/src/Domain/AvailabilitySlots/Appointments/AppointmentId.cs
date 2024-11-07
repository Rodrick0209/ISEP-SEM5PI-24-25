using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentId : EntityId
    {
        // Construtor usando Guid
        [JsonConstructor]
        public AppointmentId(Guid value) : base(value)
        {
        }

        // Construtor usando string
        public AppointmentId(string value) : base(value)
        {
        }

        // Método sobrescrito para criar o AppointmentId a partir de uma string
        override
        protected object createFromString(string text)
        {
            return new Guid(text);
        }

        // Método para retornar o valor como string
        override
        public string AsString()
        {
            Guid obj = (Guid)base.ObjValue;
            return obj.ToString();
        }

        // Método para retornar o valor como Guid
        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }
    }
}
