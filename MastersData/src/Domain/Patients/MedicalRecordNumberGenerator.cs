using System;

namespace DDDSample1.Domain.Patients
{
    public class MedicalRecordNumberGenerator
    {
        private static int lastSequentialNumber = 0;

        public static string GenerateMedicalRecordNumber()
        {
            string yearMonth = DateTime.Now.ToString("yyyyMM");
            lastSequentialNumber++;

            return $"{yearMonth}{lastSequentialNumber:D6}";
        }
    }
}