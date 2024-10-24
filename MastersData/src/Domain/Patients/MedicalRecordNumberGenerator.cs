using System;

namespace DDDSample1.Domain.Patients
{
    public class MedicalRecordNumberGenerator
    {
        private static int ExtractSequentialNumber(string medicalRecordNumber)
        {
            return int.Parse(medicalRecordNumber.Substring(6));
        }

        public static string GenerateMedicalRecordNumber(Patient lastPatientInMonth)
        {
            int sequentialNumber;
            if (lastPatientInMonth == null)
            {
                sequentialNumber = 1;
            }
            else
            {
                string lastMedicalRecordNumber = lastPatientInMonth.MedicalRecordNumber._medicalRecordNumber;
                sequentialNumber = ExtractSequentialNumber(lastMedicalRecordNumber) + 1;
            }

            string currentYearMonth = DateTime.Now.ToString("yyyyMM");
            return $"{currentYearMonth}{sequentialNumber:D6}";
        }
    }
}