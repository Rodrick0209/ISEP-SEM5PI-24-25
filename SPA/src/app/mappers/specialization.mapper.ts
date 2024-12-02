import { Specialization } from './../models/specialization';

export class SpecializationMapper {
  
    static mapToSpecialization(item: any): Specialization {
    return {
      id: item.id,
      Name: item.Name
    };
  }

  static mapToSpecializations(items: any[]): Specialization[] {
    return items.map(item => this.mapToSpecialization(item));
  }
}