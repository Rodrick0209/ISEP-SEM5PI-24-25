using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class MedicalRecord : Entity<MedicalRecordId>
    {
        public List<string> _medicalRecord;

        private MedicalRecord(){
        }

        public MedicalRecord(List<string> medicalRecord){
            this.Id = new MedicalRecordId(Guid.NewGuid());
            this._medicalRecord = medicalRecord;
        }

        public void AddMedicalRecord(string medicalRecord){
            _medicalRecord.Add(medicalRecord);
        }

    }
}