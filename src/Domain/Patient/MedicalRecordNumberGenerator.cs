using System;

namespace DDDSample1.Domain.Patient
{
    public class MedicalRecordNumberGenerator
    {

        public static string GenerateMedicalRecordNumber(DateTime registrationDate, Patient lastPatientInMonth)
        {
            string yearMonth = registrationDate.ToString("yyyyMM");
            int nextNumber = 1;

            if (lastPatientInMonth != null)
            {
                string lastNumberStr = lastPatientInMonth.MedicalRecordNumber.ToString().Substring(6);
                if (int.TryParse(lastNumberStr, out int lastNumber))
                {
                    nextNumber = lastNumber + 1;
                }
            }

            return $"{yearMonth}{nextNumber:D6}";
        }
    }
}