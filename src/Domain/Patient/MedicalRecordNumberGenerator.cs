using System;

namespace DDDSample1.Domain.Patient
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