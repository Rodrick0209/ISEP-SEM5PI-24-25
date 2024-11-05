using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRooms;

namespace DDDSample1.Infrastructure.OperationRooms
{
    internal class OperationRoomEntityTypeConfiguration : IEntityTypeConfiguration<OperationRoom>
    {
        public void Configure(EntityTypeBuilder<OperationRoom> builder)
        {
            // Definindo a chave primária
            builder.HasKey(b => b.Id);

            // Configurando o RoomNumber como um objeto próprio
            builder.OwnsOne(b => b.RoomNumber, rn =>
            {
                rn.Property(r => r.roomNumber).IsRequired(); // Assume que RoomNumber tem uma propriedade Value
                rn.HasIndex(r => r.roomNumber).IsUnique(); // Define o RoomNumber como único
            });

            // Configurando o RoomType como um objeto próprio
            builder.OwnsOne(b => b.RoomType, rt =>
            {
                rt.Property(r => r.roomType).IsRequired(); // Assume que RoomType tem uma propriedade Value
            });

            // Configurando o RoomCapacity como um objeto próprio
            builder.OwnsOne(b => b.RoomCapacity, rc =>
            {
                rc.Property(r => r.roomCapacity).IsRequired(); // Assume que RoomCapacity tem uma propriedade Capacity
            });



        }
    }
}