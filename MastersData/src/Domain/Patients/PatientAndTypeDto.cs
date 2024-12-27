using System;
using System.Collections.Generic;

#nullable enable

namespace DDDSample1.Domain.Patients
{
    public class PatientAndTypeDto
    {
        public PatientDto patient { get; set; }
        
        public string operationType { get; set; }
        
        public PatientAndTypeDto(PatientDto patient,string type)
        {
            this.patient=patient;
            this.operationType=type;
        }

        public PatientAndTypeDto()
        {
        }
    }
}