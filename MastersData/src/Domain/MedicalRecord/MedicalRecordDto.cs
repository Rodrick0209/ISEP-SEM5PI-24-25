using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Patients
{
    public class MedicalRecordDto
    {
        public Guid Id { get; set; }
        public List<string> MedicalRecord { get; set; }

        public MedicalRecordDto() { }

        public MedicalRecordDto(Guid id, List<string> medicalRecord)
        {
            this.Id = id;
            this.MedicalRecord = medicalRecord;
        }
    }
}