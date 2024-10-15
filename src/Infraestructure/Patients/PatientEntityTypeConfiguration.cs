using DDDSample1.Domain.Patient;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.Patients
{
    internal class PatientEntityTypeConfiguration : IEntityTypeConfiguration<Patient>
    {
        public void Configure(EntityTypeBuilder<Patient> builder)
        {
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.DateOfBirth);
            builder.OwnsOne(b => b.Email).HasIndex(e => e.email).IsUnique();
            builder.OwnsOne(b => b.EmergencyContact).Property(e => e.emergencyContact).IsRequired(false);
            builder.OwnsOne(b => b.FullName);
            builder.OwnsOne(b => b.PhoneNumber).HasIndex(p => p.phoneNumber).IsUnique();
            builder.OwnsOne(b => b.MedicalRecordNumber);
        }
    }
}
