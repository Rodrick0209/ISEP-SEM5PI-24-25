using System;

namespace DDDSample1.Domain.Patients
{
    public class MedicalRecordNumberGenerator
    {
        private static int lastSequentialNumber = 0;

        public static MedicalRecordNumber GenerateMedicalRecordNumber()
        {
            string yearMonth = DateTime.Now.ToString("yyyyMM");
            lastSequentialNumber++;

            return new MedicalRecordNumber($"{yearMonth}{lastSequentialNumber:D6}");
        }
    }
}