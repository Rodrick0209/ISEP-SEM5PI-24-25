using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using Domain.Staff;

namespace DDDSample1.Domain.OperationType
{
    public class Phase : IValueObject
    {
        public int duration { get; private set; }

        //public Dictionary<Specialization,int> specializations { get; private set; }

        private Phase() { }

        public Phase(int duration, Dictionary<Specialization,int> specializations)
        {
            this.duration = duration;
            //this.specializations = specializations;
        }

        public Phase(string duration)
        {
            if (!int.TryParse(duration, out int parsedDuration))
            {
                throw new ArgumentException("Invalid duration format");
            }
            this.duration = parsedDuration;
        }

        public string AsString()
        {
            return $"Phase duration: {duration}";
        }
        
    }
}