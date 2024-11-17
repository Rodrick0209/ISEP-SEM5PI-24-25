import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FilterStaffsComponent } from './filter-staffs.component';

describe('FilterStaffsComponent', () => {
  let component: FilterStaffsComponent;
  let fixture: ComponentFixture<FilterStaffsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FilterStaffsComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FilterStaffsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
