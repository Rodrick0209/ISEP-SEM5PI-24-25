import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FilterMedicalRecordEntriesComponent } from './filter-medical-record-entries.component';
import { FormsModule } from '@angular/forms';
import { By } from '@angular/platform-browser';

describe('FilterMedicalRecordEntriesComponent', () => {
  let component: FilterMedicalRecordEntriesComponent;
  let fixture: ComponentFixture<FilterMedicalRecordEntriesComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FormsModule],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(FilterMedicalRecordEntriesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should emit filterChanged event when applyFilter is called', () => {
    spyOn(component.filterChanged, 'emit');
    component.applyFilter();
    expect(component.filterChanged.emit).toHaveBeenCalledWith(component.filter);
  });

  it('should call applyFilter when onSearch is called', () => {
    spyOn(component, 'applyFilter');
    const event = new Event('submit');
    component.onSearch(event);
    expect(component.applyFilter).toHaveBeenCalled();
  });

  it('should prevent default action when onSearch is called', () => {
    const event = jasmine.createSpyObj('event', ['preventDefault']);
    component.onSearch(event);
    expect(event.preventDefault).toHaveBeenCalled();
  });
});