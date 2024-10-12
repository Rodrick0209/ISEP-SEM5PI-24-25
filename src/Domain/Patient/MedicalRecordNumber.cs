using System;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.Operation
{

    public class MedicalRecordNumber : IValueObject
    {

        public string _medicalRecordNumber { get; private set; }

        public MedicalRecordNumber(string medicalRecordNumber)
        {
            validateMedicalRecordNumber(medicalRecordNumber);
            _medicalRecordNumber = medicalRecordNumber;

        }



        private void validateMedicalRecordNumber(string medicalRecordNumber)
        {
            //Quem implementar o use case de criar o paciente tem de adicionar a regra de negocio do medical record number


        }







    }
    








}