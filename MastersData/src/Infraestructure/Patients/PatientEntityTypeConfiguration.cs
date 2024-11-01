using DDDSample1.Domain.Patients;
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
            builder.OwnsOne(b => b.FullName);
            builder.OwnsOne(b => b.PhoneNumber).HasIndex(p => p.phoneNumber).IsUnique();
            builder.OwnsOne(b => b.Address, a =>
            {
                a.OwnsOne(ad => ad.Street);
                a.OwnsOne(ad => ad.PostalCode);
                a.OwnsOne(ad => ad.City);
                a.OwnsOne(ad => ad.Country);
            });
            builder.OwnsOne(b => b.EmergencyContact, ec =>
            {
                ec.OwnsOne(e => e.Name);
                ec.OwnsOne(e => e.Email);
                ec.OwnsOne(e => e.PhoneNumber);
            });
            builder.OwnsOne(b => b.MedicalRecordNumber);
            builder.OwnsOne(b => b.MedicalHistory, mh =>
            {
                mh.OwnsOne(m => m.MedicalConditions);
            });
            builder.OwnsOne(b => b.Gender).Property(g => g.gender);
        }
    }
}
